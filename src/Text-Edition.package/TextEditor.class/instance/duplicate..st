editing keys
duplicate: aKeyboardEvent
	"Paste the current selection over the prior selection, if it is non-overlapping and
	 legal.  Undoer & Redoer: undoAndReselect."

	self closeTypeIn.
	(self hasSelection and: [self isDisjointFrom: self editingState previousInterval])
		ifTrue: "Something to duplicate"
			[self replace: self editingState previousInterval with: self selection and:
				[self selectAt: self pointIndex]]
		ifFalse:
			[morph flash].
	^true