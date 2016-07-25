inline messages
emitOr: aMessageNode

	valueTranslator visitNode: aMessageNode receiver.
	methodBuilder jumpAheadTo: #else if: false.
	methodBuilder pushLiteral: true. 
	methodBuilder jumpAheadTo: #end.
	methodBuilder jumpAheadTarget: #else.
	valueTranslator visitInlinedBlockNode: aMessageNode arguments first.
	methodBuilder jumpAheadTarget: #end.