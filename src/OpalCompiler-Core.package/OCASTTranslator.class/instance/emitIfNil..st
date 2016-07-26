inline messages
emitIfNil: aMessageNode

	valueTranslator visitNode: aMessageNode receiver.
	self isValueTranslator ifTrue: [ methodBuilder pushDup ].
	methodBuilder pushLiteral: nil.
	methodBuilder send: #==.
	methodBuilder jumpAheadTo: #else if: false.
	self isValueTranslator ifTrue: [ methodBuilder popTop ].
	self visitInlinedBlockNode: aMessageNode arguments first.
	methodBuilder jumpAheadTarget: #else.
