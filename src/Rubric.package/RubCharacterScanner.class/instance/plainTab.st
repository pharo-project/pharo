scanning
plainTab

	"This is the basic method of adjusting destX for a tab."
	destX := (alignment = self justified and: [self leadingTab not])
		ifTrue:		"embedded tabs in justified text are weird"
			[destX + (self tabWidth - (line justifiedTabDeltaFor: spaceCount)) max: destX]
		ifFalse: 
			[self nextTabXFrom: destX].
	pendingKernX := 0.
	firstDestX  := destX.