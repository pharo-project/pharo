initialization
fillOutDefinition: aDictionary
	className := (aDictionary at: '`#traitName') asSymbol.
	category := (aDictionary at: '`#category' ifAbsent: [ #Unclassified ]) asSymbol