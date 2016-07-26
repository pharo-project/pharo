protocol
valueAtNewKey: aKey put: anObject atIndex: index declareFrom: aDictionary 
	
	value valueAtNewKey: aKey put: anObject atIndex: index declareFrom: aDictionary.
	self valueChanged.