tests
loopbackTest
	"Send data from one socket to another on the local machine.
	Tests most of the socket primitives."

	"100 timesRepeat: [Socket loopbackTest]"

	| sock1 sock2 bytesToSend sendBuf receiveBuf done bytesSent bytesReceived t extraBytes packetsSent packetsRead |
	Transcript
		cr;
		show: 'starting loopback test';
		cr.
	Transcript
		show: '---------- Connecting ----------';
		cr.
	self initializeNetwork.
	sock1 := self new.
	sock2 := self new.
	sock1 listenOn: 54321.
	sock2 connectTo: NetNameResolver localHostAddress port: 54321.
	sock1 waitForConnectionFor: self standardTimeout.
	sock2 waitForConnectionFor: self standardTimeout.
	sock1 isConnected ifFalse: [self error: 'sock1 not connected'].
	sock2 isConnected ifFalse: [self error: 'sock2 not connected'].
	Transcript
		show: 'connection established';
		cr.
	bytesToSend := 5000000.
	sendBuf := String new: 5000 withAll: $x.
	receiveBuf := String new: 50000.
	done := false.
	packetsSent := packetsRead := bytesSent := bytesReceived := 0.
	t := Time millisecondsToRun: 
					[[done] whileFalse: 
							[(sock1 sendDone and: [bytesSent < bytesToSend]) 
								ifTrue: 
									[packetsSent := packetsSent + 1.
									bytesSent := bytesSent + (sock1 sendSomeData: sendBuf)].
							sock2 dataAvailable 
								ifTrue: 
									[packetsRead := packetsRead + 1.
									bytesReceived := bytesReceived + (sock2 receiveDataInto: receiveBuf)].
							done := bytesSent >= bytesToSend and: [bytesReceived = bytesSent]]].
	Transcript
		show: 'closing connection';
		cr.
	sock1 waitForSendDoneFor: self standardTimeout.
	sock1 close.
	sock2 waitForDisconnectionFor: self standardTimeout.
	extraBytes := sock2 discardReceivedData.
	extraBytes > 0 
		ifTrue: 
			[Transcript
				show: ' *** received ' , extraBytes size printString , ' extra bytes ***';
				cr].
	sock2 close.
	sock1 waitForDisconnectionFor: self standardTimeout.
	sock1 isUnconnectedOrInvalid ifFalse: [self error: 'sock1 not closed'].
	sock2 isUnconnectedOrInvalid ifFalse: [self error: 'sock2 not closed'].
	Transcript
		show: '---------- Connection Closed ----------';
		cr.
	sock1 destroy.
	sock2 destroy.
	Transcript
		show: 'loopback test done; time = ' , t printString;
		cr.
	Transcript
		show: (bytesToSend asFloat / t roundTo: 0.01) printString 
					, '* 1000 bytes/sec';
		cr.
	Transcript endEntry