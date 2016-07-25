inline messages
emitAnd: aMessageNode

	valueTranslator visitNode: aMessageNode receiver.
	methodBuilder jumpAheadTo: #else if: false.
	valueTranslator visitInlinedBlockNode: aMessageNode arguments first.
	methodBuilder jumpAheadTo: #end.
	methodBuilder jumpAheadTarget: #else.
	methodBuilder pushLiteral: false. 
	methodBuilder jumpAheadTarget: #end.