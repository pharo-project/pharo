instance creation
redo: doMessageSend undo: undoMessageSend

	| anUndoRecord |
	anUndoRecord := self new.
	anUndoRecord redoMessage: doMessageSend.
	anUndoRecord undoMessage: undoMessageSend.
	^anUndoRecord.