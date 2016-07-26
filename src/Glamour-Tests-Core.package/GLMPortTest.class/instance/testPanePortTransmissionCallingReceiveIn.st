tests
testPanePortTransmissionCallingReceiveIn
	| originPort destinationPort transmission |
	originPort := GLMTestPane new port: #originPort.
	destinationPort := GLMTestPane new port: #destinationPort.
	self assert: destinationPort seenTransmissions isEmpty.
	transmission := (GLMTransmission new)
		addActiveOrigin: originPort;
		destination: destinationPort;
		transformation: [ :x | x + 1 ].
	originPort value: 41.
	transmission transmit.
	self assert: destinationPort seenTransmissions size equals: 1.
	self assert: destinationPort seenTransmissions first origins first value equals: 41.
	self assert: destinationPort seenTransmissions first destination == destinationPort.
	self assert: destinationPort value equals: 42