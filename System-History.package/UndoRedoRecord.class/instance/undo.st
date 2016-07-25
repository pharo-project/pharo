redo-undo
undo
	^self undoMessage
		ifNotNil: [self undoMessage value].