inline messages
emitIfNotNil: aMessageNode
	| args |
	self visitNode: aMessageNode receiver.
	args := aMessageNode arguments.
	args first arguments ifNotEmpty: [ args first arguments first binding emitStore: methodBuilder ].
	methodBuilder pushDup.
	methodBuilder pushLiteral: nil.
	methodBuilder send: #==.
	methodBuilder jumpAheadTo: #end if: true.
	methodBuilder popTop.
	self visitInlinedBlockNode: args first.
	methodBuilder jumpAheadTarget: #end.