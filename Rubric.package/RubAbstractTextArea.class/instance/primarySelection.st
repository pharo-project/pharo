accessing selection
primarySelection
	^ self submorphThat: [ :sm | sm isKindOf: RubPrimarySelectionMorph ] ifNone:  [ ]
