inline messages
emitWhileFalse: aMessageNode

	super emitWhileFalse: aMessageNode.
	methodBuilder pushLiteral: nil.
