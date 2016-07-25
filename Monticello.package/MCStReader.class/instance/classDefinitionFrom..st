reading
classDefinitionFrom: aRingClass
	| tokens traitCompositionString lastIndex classTraitCompositionString |
	tokens := aRingClass definitionSource parseLiterals.
	traitCompositionString := (aRingClass definitionSource readStream
		match: 'uses:';
		upToAll: 'instanceVariableNames:') trimBoth.
	classTraitCompositionString := (aRingClass theMetaClass definitionSource asString readStream
		match: 'uses:';
		upToAll: 'instanceVariableNames:') trimBoth.
	traitCompositionString isEmpty ifTrue: [traitCompositionString := '{}'].
	classTraitCompositionString isEmpty ifTrue: [classTraitCompositionString := '{}'].
	lastIndex := tokens size.
	^ MCClassDefinition
		name: (tokens at: 3)
		superclassName: (tokens at: 1)
		traitComposition: traitCompositionString
		classTraitComposition: classTraitCompositionString
		category: (tokens at: lastIndex)
		instVarNames: ((tokens at: lastIndex - 6) findTokens: ' ')
		classVarNames: ((tokens at: lastIndex - 4) findTokens: ' ')
		poolDictionaryNames: ((tokens at: lastIndex - 2) findTokens: ' ')
		classInstVarNames: (self classInstVarNamesFor: aRingClass)
		type: (self typeOfSubclass: (tokens at: 2))
		comment: (self commentFor: aRingClass)
		commentStamp: (self commentStampFor: aRingClass)