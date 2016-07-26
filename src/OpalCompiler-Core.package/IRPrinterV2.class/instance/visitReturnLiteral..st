visiting
visitReturnLiteral: lit

	| object |
	object := lit literal.
	stream nextPutAll: 'returnLiteral: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.
