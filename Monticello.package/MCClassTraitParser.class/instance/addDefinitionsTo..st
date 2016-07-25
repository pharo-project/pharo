actions
addDefinitionsTo: aCollection
	| tokens  definition traitCompositionString |
	tokens := source parseLiterals.
	traitCompositionString := (source readStream
		match: 'uses:';
		upToEnd) trimBoth.
	definition := MCClassTraitDefinition
		baseTraitName: (tokens at: 1) 
		classTraitComposition: traitCompositionString.
	aCollection add: definition
