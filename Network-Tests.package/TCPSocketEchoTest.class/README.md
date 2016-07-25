TCPSocketEchoTest is both a unit test and an example.
It implements and tests a TCP echo service.
Input is read and send back in response.

You can also run the example manually,
by inspecting each expression separately.

  TCPSocketEchoTest new runServer.
  TCPSocketEchoTest new clientSend: 'Hello @ ', Time now asString.
  TCPSocketEchoTest new clientSend: #quit.

Each TCP client connection creates a worker process on the server handling the connection until it is closed. Each worker process reads input one time and sends it back.

The server runs until it receives quit as input. If necessary, use the Process Browser to terminate a running server.

Note: this example deliberately does not use SocketStream to show how to use Socket directly. In practice however, you should use SocketStream.