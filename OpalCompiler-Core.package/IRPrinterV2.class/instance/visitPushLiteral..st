visiting
visitPushLiteral: lit

	| object |
	object := lit literal.
	stream nextPutAll: 'pushLiteral: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.
