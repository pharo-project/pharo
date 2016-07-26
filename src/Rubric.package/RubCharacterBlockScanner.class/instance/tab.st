stop conditions
tab
	| currentX |
	currentX := (alignment = Justified and: [self leadingTab not])
		ifTrue:		"imbedded tabs in justified text are weird"
			[destX + (self tabWidth - (line justifiedTabDeltaFor: spaceCount)) max: destX]
		ifFalse:
			[self nextTabXFrom: destX].
	lastSpaceOrTabExtent := lastCharacterExtent copy.
	self lastSpaceOrTabExtentSetX: (currentX - destX max: 0).
	currentX >= characterPoint x
		ifTrue: 
			[lastCharacterExtent := lastSpaceOrTabExtent copy.
			^ self crossedX].
	destX := currentX.
	lastIndex := lastIndex + 1.
	^false