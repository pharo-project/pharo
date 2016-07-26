visiting
visitPushLiteralVariable: var
	
	| object |
	object := var association.
	stream nextPutAll: 'pushLiteralVariable: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.