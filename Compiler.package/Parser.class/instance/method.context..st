expression types
method: doit context: ctxt
	" pattern [ | temporaries ] block => MethodNode."

	| sap blk prim temps messageComment methodNode |
	properties := AdditionalMethodState new.
	sap := self pattern: doit inContext: ctxt.
	"sap={selector, arguments, precedence}"
	properties selector: (sap at: 1).
	encoder selector: (sap at: 1).
	(sap at: 2) do: [:argNode | argNode beMethodArg].
	doit ifFalse: [self pragmaSequence].
	temps := self temporaries.
	messageComment := currentComment.
	currentComment := nil.
	doit ifFalse: [self pragmaSequence].
	prim := self pragmaPrimitives.
	self statements: #() innerBlock: doit.
	blk := parseNode.
	doit ifTrue: [blk returnLast]
		ifFalse: [blk returnSelfIfNoOther: encoder].
	hereType == #doIt ifFalse: [^self expected: 'Nothing more'].
	self interactive ifTrue: [self removeUnusedTemps].
	methodNode := self newMethodNode comment: messageComment.
	^methodNode
		selector: (sap at: 1)
		arguments: (sap at: 2)
		precedence: (sap at: 3)
		temporaries: temps
		block: blk
		encoder: encoder
		primitive: prim
		properties: properties