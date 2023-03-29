#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
import queue
import threading
import traceback
import sys
from functools import wraps
from epc.server import ThreadingEPCServer
from utils import (init_epc_client, eval_in_emacs, is_valid_ip_path, logger, close_epc_client,
                   message_emacs, string_to_base64, epc_arg_transformer, eval_sexp_in_emacs)
import paramiko
import glob
import os
import json

def threaded(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        thread = threading.Thread(target=func, args=args, kwargs=kwargs)
        thread.start()
        if hasattr(args[0], 'thread_queue'):
            args[0].thread_queue.append(thread)
    return wrapper

class Nova:
    def __init__(self, args):
        # Init EPC client port.
        init_epc_client(int(args[0]))

        # Build EPC server.
        self.server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
        # self.server.logger.setLevel(logging.DEBUG)
        self.server.allow_reuse_address = True

        # ch = logging.FileHandler(filename=os.path.join(nova_config_dir, 'epc_log.txt'), mode='w')
        # formatter = logging.Formatter('%(asctime)s | %(levelname)-8s | %(lineno)04d | %(message)s')
        # ch.setFormatter(formatter)
        # ch.setLevel(logging.DEBUG)
        # self.server.logger.addHandler(ch)
        # self.server.logger = logger

        self.server.register_instance(self)  # register instance functions let elisp side call

        # Start EPC server with sub-thread, avoid block Qt main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()
        
        # All Emacs request running in event_loop.
        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        # All SSH server response running in message_thread.
        self.message_queue = queue.Queue()
        self.message_thread = threading.Thread(target=self.message_dispatcher)
        self.message_thread.start()

        self.lsp_sender_queue = queue.Queue()
        self.lsp_sender_thread = threading.Thread(target=self.lsp_sender_dispatcher)
        self.lsp_sender_thread.start()

        self.lsp_receiver_queue = queue.Queue()
        self.lsp_receiver_thread = threading.Thread(target=self.lsp_receiver_dispatcher)
        self.lsp_receiver_thread.start()

        # Build thread queue.
        self.thread_queue = []

        # Client dict.
        self.client_dict = {}
        self.lsp_client_dict = {}

        # Pass epc port and webengine codec information to Emacs when first start nova.
        eval_in_emacs('nova--first-start', self.server.server_address[1])

        # event_loop never exit, simulation event loop.
        self.event_loop.join()

    def event_dispatcher(self):
        try:
            while True:
                data = self.event_queue.get(True)

                client = self.get_client(data["host"], 9999)
                client.send_message(data["message"])

                self.event_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def message_dispatcher(self):
        try:
            while True:
                message = self.message_queue.get(True)
                self.handle_message(message)
                self.message_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def lsp_sender_dispatcher(self):
        try:
            while True:
                data = self.lsp_sender_queue.get(True)

                client = self.get_client(data["host"], 9998)
                client.send_message(data["message"])

                self.lsp_sender_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def lsp_receiver_dispatcher(self):
        try:
            while True:
                message = self.lsp_receiver_queue.get(True)

                data = json.loads(message)
                if data["command"] == "eval-in-emacs":
                    eval_sexp_in_emacs(data["sexp"])

                self.lsp_receiver_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def receive_client_message(self, message, server_port):
        if server_port == 9999:
            self.message_queue.put(message)
        elif server_port == 9998:
            self.lsp_receiver_queue.put(message)

    @threaded
    def lsp_request(self, remote_file_host, remote_file_path, method, args):
        if method == "change_file":
            self.send_message(remote_file_host, {
                "command": "change_file",
                "server": remote_file_host,
                "path": remote_file_path,
                "args": list(map(epc_arg_transformer, args))
            })

        self.send_lsp_message(remote_file_host, {
            "command": "lsp_request",
            "server": remote_file_host,
            "path": remote_file_path,
            "method": method,
            "args": list(map(epc_arg_transformer, args))
        })

    @threaded
    def save_file(self, remote_file_host, remote_file_path):
        self.send_message(remote_file_host, {
            "command": "save_file",
            "server": remote_file_host,
            "path": remote_file_path
        })

    @threaded
    def close_file(self, remote_file_host, remote_file_path):
        self.send_message(remote_file_host, {
            "command": "close_file",
            "server": remote_file_host,
            "path": remote_file_path
        })

    @threaded
    def handle_message(self, message):
        data = json.loads(message)
        command = data["command"]

        if command == "open_file":
            if "error" in data:
                message_emacs(data["error"])
            else:
                path = data["path"]
                eval_in_emacs("nova-open-file--response", data["server"], path, string_to_base64(data["content"]))
                message_emacs(f"Open file {path} done.")

    def get_client(self, server_host, server_port):
        client_id = f"{server_host}:{server_port}"

        if client_id in self.client_dict:
            client = self.client_dict[client_id]
        else:
            client = Client(server_host,
                            "root",
                            server_port,
                            lambda message: self.receive_client_message(message, server_port))
            client.start()

            self.client_dict[client_id] = client

        return client

    @threaded
    def open_file(self, path):
        if is_valid_ip_path(path):
            [server_host, server_path] = path.split(":")

            message_emacs(f"Open file {server_path}...")

            self.send_message(server_host, {
                "command": "open_file",
                "server": server_host,
                "path": server_path
            })
        else:
            message_emacs("Please input valid path match rule: 'ip:/path/file'.")

    def send_message(self, host, message):
        self.event_queue.put({
            "host": host,
            "message": message
        })

    def send_lsp_message(self, host, message):
        self.lsp_sender_queue.put({
            "host": host,
            "message": message
        })

    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()

class Client(threading.Thread):
    def __init__(self, ssh_host, ssh_user, server_port, callback):
        threading.Thread.__init__(self)

        self.ssh_host = ssh_host
        self.ssh_user = ssh_user
        self.server_port = server_port

        self.callback = callback

        self.ssh = self.connect_ssh()
        self.transport = self.ssh.get_transport()
        self.chan = self.transport.open_channel("direct-tcpip", (self.ssh_host, self.server_port), ('0.0.0.0', 0))

    def ssh_pub_key(self):
        ssh_dir = os.path.expanduser('~/.ssh')
        pub_keys = glob.glob(os.path.join(ssh_dir, '*.pub'))
        return pub_keys[0]

    def connect_ssh(self):
        ssh = paramiko.SSHClient()
        ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        ssh.connect(self.ssh_host, username=self.ssh_user, key_filename=self.ssh_pub_key())
        return ssh

    def send_message(self, message):
        try:
            data = json.dumps(message)
            self.chan.sendall(f"{data}\n".encode("utf-8"))
        except:
            logger.error(traceback.format_exc())

    def run(self):
        chan_file = self.chan.makefile('r')
        while True:
            message = chan_file.readline().strip()
            if not message:
                break
            self.callback(message)
        self.chan.close()

if __name__ == "__main__":
    if len(sys.argv) >= 3:
        import cProfile
        profiler = cProfile.Profile()
        profiler.run("Nova(sys.argv[1:])")
    else:
        Nova(sys.argv[1:])
