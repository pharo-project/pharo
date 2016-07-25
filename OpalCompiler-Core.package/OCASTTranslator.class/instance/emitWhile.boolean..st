inline messages factored
emitWhile: aMessageNode boolean: aBoolean

	methodBuilder jumpBackTarget: #begin.
	valueTranslator visitInlinedBlockNode: aMessageNode receiver.
	methodBuilder jumpAheadTo: #end if: aBoolean.
	aMessageNode arguments ifNotEmpty: [ 
			effectTranslator visitInlinedBlockNode: aMessageNode arguments first].
	methodBuilder jumpBackTo: #begin.
	methodBuilder jumpAheadTarget: #end.