accessing
leftMarginForAlignment: alignmentCode
	alignmentCode = RightFlush ifTrue: [^ self left + paddingWidth].
	alignmentCode = Centered ifTrue: [^ self left + (paddingWidth//2)].
	^ self left  "leftFlush and justified"