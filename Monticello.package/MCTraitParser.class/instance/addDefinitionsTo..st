actions
addDefinitionsTo: aCollection
	| tokens  definition traitCompositionString |
	tokens := source parseLiterals.
	traitCompositionString := (source readStream
		match: 'uses:';
		upToAll: 'category:') trimBoth.
	definition := MCTraitDefinition
		name: (tokens at: 3) 
		traitComposition: traitCompositionString
		category:  tokens last
		comment:  ''  
		commentStamp:   ''.
	aCollection add: definition.