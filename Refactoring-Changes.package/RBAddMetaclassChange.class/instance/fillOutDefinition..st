initialization
fillOutDefinition: aDictionary
	className := (aDictionary at: '`className') asSymbol.
	classInstanceVariableNames := self namesIn: (aDictionary
		at: '`#instanceVariableNames'
		ifAbsent: [ String new ])