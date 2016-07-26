*Morphic-Widgets-Windows
nextMorphInWindow
	"Answer the next morph in the window. Traverse
	from the receiver to its first child or next sibling or owner's next sibling etc."

	^self hasSubmorphs
		ifTrue: [self submorphs first]
		ifFalse: [self nextMorphAcrossInWindow]