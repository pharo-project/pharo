inline messages
emitIfNotNil: aMessageNode
	| args |
	valueTranslator visitNode: aMessageNode receiver.
	args := aMessageNode arguments.
	args first arguments ifNotEmpty: [ args first arguments first binding emitStore: methodBuilder ].
	methodBuilder pushLiteral: nil.
	methodBuilder send: #==.
	methodBuilder jumpAheadTo: #end if: true.
	self visitInlinedBlockNode: args first.
	methodBuilder jumpAheadTarget: #end.
	