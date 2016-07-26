accessing
allTraits
	^self traits flatCollect: [:trait |
		trait hasTraitComposition
			ifTrue: [trait traitComposition allTraits copyWith: trait]
			ifFalse: [Array with: trait]]