accessing
addTransmission: aTransmission 
	aTransmission browser: self.
	^ self transmissions addLast: aTransmission