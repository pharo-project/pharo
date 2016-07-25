visiting
visitPopIntoLiteralVariable: litVar
	
	| object |
	object := litVar association.
	stream nextPutAll: 'popIntoLiteralVariable: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.