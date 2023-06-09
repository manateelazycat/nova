import socket
import queue
import os
import json
import traceback
import threading

class Server:
    def __init__(self, host, port):
        self.host = host
        self.port = port
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

        self.server.bind((self.host, self.port))
        self.server.listen(5)
        print(f"[*] Listening on {self.host}:{self.port}")

        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        self.message_queue = queue.Queue()
        self.message_thread = threading.Thread(target=self.message_dispatcher)
        self.message_thread.start()

        self.file_dict = {}

        self.event_loop.join()

    def event_dispatcher(self):
        try:
            while True:
                client_socket, client_address = self.server.accept()
                print(f"[*] Accepted connection from {client_address[0]}:{client_address[1]}")

                client_handler = threading.Thread(target=self.handle_client, args=(client_socket,))
                client_handler.start()
        except:
            print(traceback.format_exc())

    def message_dispatcher(self):
        try:
            while True:
                client_socket = self.message_queue.get(True)
                self.handle_client(client_socket)
                self.message_queue.task_done()
        except:
            print(traceback.format_exc())

    def handle_client(self, client_socket):
        client_file = client_socket.makefile('r')
        while True:
            message = client_file.readline().strip()
            if not message:
                break
            self.handle_message(message, client_socket)
        client_socket.close()

    def handle_message(self, message, client_socket):
        print(f"[*] '{message}'")

        data = json.loads(message)
        command = data["command"]

        if command == "open_file":
            self.handle_open_file(data, client_socket)
        elif command == "save_file":
            self.handle_save_file(data, client_socket)
        elif command == "close_file":
            self.handle_close_file(data, client_socket)
        elif command == "change_file":
            self.handle_change_file(data, client_socket)

    def handle_open_file(self, data, client_socket):
        path = data["path"]
        server = data["server"]

        if os.path.exists(path):
            with open(path) as f:
                content = f.read()

                response = {
                    "command": "open_file",
                    "server": server,
                    "path": path,
                    "content": content
                }

                self.file_dict[path] = content
        else:
            response = {
                "command": "open_file",
                "server": server,
                "path": path,
                "content": "",
                "error": f"Cannot found file {path} on server."
            }

        response_data = json.dumps(response)
        client_socket.send(f"{response_data}\n".encode("utf-8"))

    def handle_change_file(self, data, client_socket):
        path = data["path"]
        if path not in self.file_dict:
            with open(path) as f:
                self.file_dict[path] = f.read()

        content = self.file_dict[path]

        start_line = data["args"][0]['line']
        start_char = data['args'][0]['character']
        end_line = data['args'][1]['line']
        end_char = data['args'][1]['character']

        start_pos = get_position(content, start_line, start_char)
        end_pos = get_position(content, end_line, end_char)

        content = content[:start_pos] + data['args'][3] + content[end_pos:]

        self.file_dict[path] = content

        print(f"###\n{content}###\n")

    def handle_save_file(self, data, client_socket):
        path = data["path"]

        if path in self.file_dict:
            with open(path, 'w') as file:
                file.write(self.file_dict[path])
                print(f"Write data to file {path}")
        else:
            print(f"Write file {path} because path not exist in file_dict somehow.")

    def handle_close_file(self, data, client_socket):
        path = data["path"]

        if path in self.file_dict:
            self.file_dict[path] = ""
            print(f"Close file {path}")

def get_position(content, line, character):
    lines = content.split('\n')
    position = sum(len(lines[i]) + 1 for i in range(line)) + character
    return position

Server("0.0.0.0", 9999)
