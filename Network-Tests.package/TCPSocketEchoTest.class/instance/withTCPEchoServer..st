private
withTCPEchoServer: block
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