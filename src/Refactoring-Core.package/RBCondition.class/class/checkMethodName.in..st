utilities
checkMethodName: aName in: aClass 
	^aName isString and: [RBScanner isSelector: aName]