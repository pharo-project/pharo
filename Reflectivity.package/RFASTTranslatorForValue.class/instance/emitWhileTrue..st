inline messages
emitWhileTrue: aMessageNode

	super emitWhileTrue: aMessageNode.
	methodBuilder pushLiteral: nil.
