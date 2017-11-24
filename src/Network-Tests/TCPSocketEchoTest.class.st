"
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
"
Class {
	#name : #TCPSocketEchoTest,
	#superclass : #TestCase,
	#category : #'Network-Tests-Kernel'
}

{ #category : #testing }
TCPSocketEchoTest >> clientSend: message [
	"Send message to the local TCP echo service and return the result"
	
	| socket |
	socket := Socket newTCP.
	^ [ 
		socket connectTo: self localhost port: self port.
		socket sendData: message.
		socket receiveData ] 
			ensure: [ socket closeAndDestroy ]
]

{ #category : #accessing }
TCPSocketEchoTest >> localhost [
	"Return the host address where the TCP echo service runs, the local host address"
	
	^ NetNameResolver localHostAddress
]

{ #category : #accessing }
TCPSocketEchoTest >> port [
	"Return the port where the TCP echo service runs"

	^ 6666
]

{ #category : #testing }
TCPSocketEchoTest >> runServer [
	"Run and return a local TCP echo server"
	
	^ self withTCPEchoServer: [ :process | process ]
]

{ #category : #testing }
TCPSocketEchoTest >> testEcho [
	| socket message result |
	socket := Socket newTCP.
	[ 
		self withTCPEchoServer: [ :process |
			message := 'Testing ', 99 atRandom asString. 
			socket connectTo: self localhost port: self port.
			socket sendData: message.
			result := socket receiveData.
			self assert: result equals: message ]  
	] ensure: [ socket closeAndDestroy ].
	"At this point the server is still running, ask it to kill itself"
	self clientSend: #quit
]

{ #category : #private }
TCPSocketEchoTest >> withTCPEchoServer: block [
	"Run a local TCP echo server on localhost:port and execute block.
	Optionally pass the new process to block. When quit is received, stop the server"
	
	| serverSocket serverLoop process |
	serverSocket := Socket newTCP.
	serverSocket listenOn: self port backlogSize: 10.
	serverLoop := true.
	process := [ 
		[ [ serverLoop ] whileTrue: [ 
			(serverSocket waitForAcceptFor: 60) 
				ifNotNil: [ :clientSocket | | result |
					[ [ 
						result := clientSocket receiveData.
						result asLowercase = #quit
							ifTrue: [ clientSocket sendData: 'Quiting'. serverLoop := false ]
							ifFalse: [ clientSocket sendData: result ] ]
						ensure: [ clientSocket closeAndDestroy ] ] 
							forkAt: Processor lowIOPriority named: 'TCP echo connection' ] ] ]
				ensure: [ serverSocket closeAndDestroy ] ] 
					forkAt: Processor highIOPriority named: 'TCP echo server'.
	^ block cull: process
]
