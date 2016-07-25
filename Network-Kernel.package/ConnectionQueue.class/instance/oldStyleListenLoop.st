private
oldStyleListenLoop
	"Private! This loop is run in a separate process. It will establish up to maxQueueLength connections on the given port."
	"Details: When out of sockets or queue is full, retry more frequently, since a socket may become available, space may open in the queue, or a previously queued connection may be aborted by the client, making it available for a fresh connection."
	"Note: If the machine is disconnected from the network while the server is running, the currently waiting socket will go from 'isWaitingForConnection' to 'unconnected', and attempts to create new sockets will fail. When this happens, delete the broken socket and keep trying to create a socket in case the network connection is re-established. Connecting and disconnecting was tested under PPP on Mac system 8.1. It is not if this will work on other platforms."

	[true] whileTrue: [
		((socket == nil) and: [connections size < maxQueueLength]) ifTrue: [
			"try to create a new socket for listening"
			socket := Socket createIfFail: [nil]].

		socket == nil
			ifTrue: [(Delay forMilliseconds: 100) wait]
			ifFalse: [
				socket isUnconnected ifTrue: [socket listenOn: portNumber].
				[socket waitForConnectionFor: 10]
					on: ConnectionTimedOut
					do: [:ex |
						socket isConnected
							ifTrue: [  "connection established"
								accessSema critical: [connections addLast: socket].
								socket := nil]
							ifFalse: [
								socket isWaitingForConnection
									ifFalse: [socket destroy. socket := nil]]]].  "broken socket; start over"
		self pruneStaleConnections].
