accessing
receive: aTransmission in: aContext
	self seenTransmissions addLast: aTransmission. 
	^super receive: aTransmission in: aContext