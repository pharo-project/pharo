scripting
newFirstTransmission
	^ firstTransmission 
		ifNil: [ self newTransmission ]
		ifNotNil: [ self firstTransmission copy ]