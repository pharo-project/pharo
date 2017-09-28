accessing
classTraitCompositionOf: aBuilder 
	^ aBuilder classTraitComposition
		ifNil: [ (aBuilder buildEnvironment hasClassNamed: aBuilder name)
				ifTrue: [ (aBuilder buildEnvironment classNamed: aBuilder name) class traitComposition ]
				ifFalse: [ '' asTraitComposition ] ]