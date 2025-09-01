#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <winsock2.h>

#pragma comment(lib, "ws2_32.lib")

#define PORT 8080
#define BUFFER_SIZE 1024

int main(void) {
    WSADATA wsaData;
    SOCKET server_fd = INVALID_SOCKET, client_fd = INVALID_SOCKET;
    struct sockaddr_in server_addr, client_addr;
    int addrlen = (int)sizeof(client_addr);
    char buffer[BUFFER_SIZE];

    // Initialize Winsock
    if (WSAStartup(MAKEWORD(2,2), &wsaData) != 0) {
        fprintf(stderr, "WSAStartup failed: %d\n", WSAGetLastError());
        return 1;
    }

    // Create socket
    server_fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (server_fd == INVALID_SOCKET) {
        fprintf(stderr, "socket() failed: %d\n", WSAGetLastError());
        WSACleanup();
        return 1;
    }

    // Allow quick reuse of the address
    {
        int opt = 1;
        setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, (const char*)&opt, sizeof(opt));
    }

    // Bind
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;
    server_addr.sin_port = htons(PORT);
    if (bind(server_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) == SOCKET_ERROR) {
        fprintf(stderr, "bind() failed: %d\n", WSAGetLastError());
        closesocket(server_fd);
        WSACleanup();
        return 1;
    }

    // Listen
    if (listen(server_fd, 1) == SOCKET_ERROR) {
        fprintf(stderr, "listen() failed: %d\n", WSAGetLastError());
        closesocket(server_fd);
        WSACleanup();
        return 1;
    }
    printf("Listening on port %d...\n", PORT);

    // Accept and echo
    client_fd = accept(server_fd, (struct sockaddr*)&client_addr, &addrlen);
    if (client_fd == INVALID_SOCKET) {
        fprintf(stderr, "accept() failed: %d\n", WSAGetLastError());
        closesocket(server_fd);
        WSACleanup();
        return 1;
    }

    printf("Client connected.\n");

    /* Buffer input and echo only after a full line (newline) is received.
       This avoids echoing each character when clients (like telnet) send
       characters immediately as you type. */
    char linebuf[BUFFER_SIZE];
    int line_pos = 0;
    int stop = 0;
    while (!stop) {
        int bytes = recv(client_fd, buffer, BUFFER_SIZE, 0);
        if (bytes == 0) {
            // connection closed gracefully by client
            break;
        } else if (bytes == SOCKET_ERROR) {
            fprintf(stderr, "recv() failed: %d\n", WSAGetLastError());
            break;
        }

        for (int i = 0; i < bytes; ++i) {
            char c = buffer[i];

            // Append to line buffer (drop if overflow would occur)
            if (line_pos < BUFFER_SIZE - 1) {
                linebuf[line_pos++] = c;
            } else {
                // Buffer full: flush what we have (best-effort) and reset
                int sent = 0;
                while (sent < line_pos) {
                    int s = send(client_fd, linebuf + sent, line_pos - sent, 0);
                    if (s == SOCKET_ERROR) {
                        fprintf(stderr, "send() failed during flush: %d\n", WSAGetLastError());
                        stop = 1; break;
                    }
                    sent += s;
                }
                line_pos = 0;
                if (stop) break;
                // try to store current char after flush
                if (c != '\r') linebuf[line_pos++] = c;
            }

            // If we hit a newline, send the whole line back
            if (c == '\n') {
                int total = line_pos;
                int sent = 0;
                while (sent < total) {
                    int s = send(client_fd, linebuf + sent, total - sent, 0);
                    if (s == SOCKET_ERROR) {
                        fprintf(stderr, "send() failed: %d\n", WSAGetLastError());
                        stop = 1; break;
                    }
                    sent += s;
                }
                line_pos = 0;
                if (stop) break;
            }
        }
    }
    printf("Client disconnected.\n");

    shutdown(client_fd, SD_SEND);
    closesocket(client_fd);
    closesocket(server_fd);
    WSACleanup();
    return 0;
}