UDPSocketEchoTest is both a unit test and an example.
It implements and tests a UDP echo service.
Each datagram sent to it is echoed back as is.

You can also run the example manually,
by inspecting each expression separately.

  UDPSocketEchoTest new runServer.
  UDPSocketEchoTest new clientSend: 'Hello @ ', Time now asString.
  UDPSocketEchoTest new clientSend: #quit.

The server runs until it receives quit as message. If necessary, use the Process Browser to terminate a running server.