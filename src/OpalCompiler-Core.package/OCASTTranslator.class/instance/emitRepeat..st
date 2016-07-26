inline messages
emitRepeat: aMessageNode
	| block |
	block := aMessageNode receiver.
	methodBuilder jumpBackTarget: #start.
	effectTranslator visitInlinedBlockNode: block.
	methodBuilder jumpBackTo: #start.
	self isValueTranslator ifTrue: [ methodBuilder pushLiteral: nil ]