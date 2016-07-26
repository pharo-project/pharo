accessing
receive: aTransmission 
	 
	self seenTransmissions addLast: aTransmission. 
	^super receive: aTransmission