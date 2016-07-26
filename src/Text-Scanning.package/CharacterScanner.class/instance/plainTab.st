private
plainTab
	"This is the basic method of adjusting destX for a tab.
	Answer the next destX"
	pendingKernX := 0.
	^(alignment = Justified and: [self leadingTab not])
		ifTrue:		"embedded tabs in justified text are weird"
			[destX + (textStyle tabWidth - (line justifiedTabDeltaFor: spaceCount)) max: destX]
		ifFalse: 
			[textStyle nextTabXFrom: destX
				leftMargin: leftMargin
				rightMargin: rightMargin].