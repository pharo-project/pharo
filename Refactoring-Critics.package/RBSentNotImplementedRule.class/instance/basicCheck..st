running
basicCheck: aMethod
	^ aMethod messages anySatisfy: [ :each | each isSelectorSymbol not ]