protocol
selectMethod: aMethod
	aMethod
		ifNotNil: [ self removeSelectionOf: aMethod.
			methodsSelection add: aMethod ]