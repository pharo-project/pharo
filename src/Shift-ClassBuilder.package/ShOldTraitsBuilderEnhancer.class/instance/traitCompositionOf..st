accessing
traitCompositionOf: aBuilder
	^ aBuilder traitComposition
		ifNil: [ (aBuilder buildEnvironment hasClassNamed: aBuilder name)
				ifTrue: [ (aBuilder buildEnvironment classNamed: aBuilder name) traitComposition ]
				ifFalse: [ '' asTraitComposition ] ]