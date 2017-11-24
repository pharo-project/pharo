"
UDPSocketEchoTest is both a unit test and an example.
It implements and tests a UDP echo service.
Each datagram sent to it is echoed back as is.

You can also run the example manually,
by inspecting each expression separately.

  UDPSocketEchoTest new runServer.
  UDPSocketEchoTest new clientSend: 'Hello @ ', Time now asString.
  UDPSocketEchoTest new clientSend: #quit.

The server runs until it receives quit as message. If necessary, use the Process Browser to terminate a running server.
"
Class {
	#name : #UDPSocketEchoTest,
	#superclass : #TestCase,
	#category : #'Network-Tests-Kernel'
}

{ #category : #testing }
UDPSocketEchoTest >> clientSend: message [
	"Send message to the local UDP echo service and return the result"
	
	| socket |
	socket := Socket newUDP.
	^ [ 
		socket sendUDPData: message toHost: self localhost port: self port.
		self receiveUDPDataFrom: socket ] 
			ensure: [ socket closeAndDestroy ]
]

{ #category : #accessing }
UDPSocketEchoTest >> localhost [
	"Return the host address where the UDP echo service runs, the local host address"
	
	^ NetNameResolver localHostAddress
]

{ #category : #accessing }
UDPSocketEchoTest >> port [
	"Return the port where the UDP echo service runs"

	^ 6666
]

{ #category : #private }
UDPSocketEchoTest >> receiveUDPDataFrom: socket [
	"Receive a UDP datagram from socket, return a 3-element array:
	{ contents. source host address. source port }
	We wait indefinitely for incoming data."
	
	| buffer result |
	buffer := String new: 256.
	socket waitForData.
	result := socket receiveUDPDataInto: buffer.
	^ { buffer copyFrom: 1 to: result first. result second asSocketAddress. result third }
]

{ #category : #testing }
UDPSocketEchoTest >> runServer [
	"Run and return a local UDP echo server"
	
	^ self withUDPEchoServer: [ :process | process ]
]

{ #category : #testing }
UDPSocketEchoTest >> testEcho [
	| socket message result |
	
	socket := Socket newUDP.
	[ 
		self withUDPEchoServer: [ :process |
			message := 'Testing ', 99 atRandom asString. 
			socket sendUDPData: message toHost: self localhost port: self port.
			result := self receiveUDPDataFrom: socket.
			self assert: result first equals: message.
			self assert: result second equals: self localhost.
			self assert: result third equals: self port.
			socket sendUDPData: #quit toHost: self localhost port: self port ]  
	] ensure: [ socket closeAndDestroy ]
]

{ #category : #private }
UDPSocketEchoTest >> withUDPEchoServer: block [
	"Run a local UDP echo server on localhost:port and execute block.
	Optionally pass the new process to block. When quit is received, stop the server"
	
	| socket loop result process |
	socket := Socket newUDP setPort: self port; yourself.
	loop := true.
	process := [ 
		[ [ loop ] whileTrue: [ 
			result := self receiveUDPDataFrom: socket.
			"echo what we received to where it came from"
			socket sendUDPData: result first toHost: result second port: result third.
			result first asLowercase = #quit ifTrue: [ loop := false ] ] ]
				ensure: [ socket closeAndDestroy ] ] 
					forkAt: Processor userBackgroundPriority named: 'UDP echo server'.
	^ block cull: process
]
