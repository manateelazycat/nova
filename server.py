import socket
import os
import json
from threading import Thread

class Server:
    def __init__(self, host, port):
        self.host = host
        self.port = port
        self.server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

    def start(self):
        self.server.bind((self.host, self.port))
        self.server.listen(5)
        print(f"[*] Listening on {self.host}:{self.port}")

        while True:
            client_socket, client_address = self.server.accept()
            print(f"[*] Accepted connection from {client_address[0]}:{client_address[1]}")
            client_handler = Thread(target=self.handle_client, args=(client_socket,))
            client_handler.start()

    def handle_client(self, client_socket):
        request = b''
        while True:
            data = client_socket.recv(1024)
            if not data:
                break
            request += data

            if request.endswith(b'\n'):
                message = request.decode('utf-8').rstrip()
                self.handle_message(message, client_socket)
                request = b''

        client_socket.close()

    def handle_message(self, message, client_socket):
        print(f"[*] {message}")

        data = json.loads(message)
        command = data["command"]

        if command == "open_file":
            path = data["path"]

            if os.path.exists(path):
                with open(path) as f:
                    response = {
                        "command": "open_file",
                        "path": path,
                        "content": f.read()
                    }
            else:
                response = {
                    "command": "open_file",
                    "path": path,
                    "content": "",
                    "error": f"Cannot found file {path} on server."
                }

            response_data = json.dumps(response)
            client_socket.send(f"{response_data}\n".encode("utf-8"))

server = Server("0.0.0.0", 9999)
server.start()
