stop conditions
tab
	| nextDestX |
	nextDestX := self plainTab.
	lastCharacterWidth := nextDestX - destX max: 0.
	nextDestX >= characterPoint x
		ifTrue: 
			[^ self crossedX].
	destX := nextDestX.
	lastIndex := lastIndex + 1.
	^false