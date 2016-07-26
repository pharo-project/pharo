inline messages
emitIfTrue: aMessageNode

	valueTranslator visitNode: aMessageNode receiver.
	methodBuilder jumpAheadTo: #else if: false.	
		self visitInlinedBlockNode: aMessageNode arguments first.
	methodBuilder jumpAheadTarget: #else.