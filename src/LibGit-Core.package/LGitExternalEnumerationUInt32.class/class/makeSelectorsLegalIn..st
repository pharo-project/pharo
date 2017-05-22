enum declaration
makeSelectorsLegalIn: aDictionary
	aDictionary associationsDo: [ :nameToValue |
		| selector |
		selector := nameToValue key asLowercase asSymbol.
		nameToValue key: selector ]