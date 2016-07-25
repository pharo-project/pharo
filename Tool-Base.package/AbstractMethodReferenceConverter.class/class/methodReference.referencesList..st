instance creation
methodReference: aMessage referencesList: aListOfReferences
	^ (self method: aMessage)
		referencesList: aListOfReferences;
		yourself