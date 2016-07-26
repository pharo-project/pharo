visiting
visitStoreLiteralVariable: var

	| object |
	object := var association.
	stream nextPutAll: 'storeLiteralVariable: '.
	object isVariableBinding ifTrue: [^ stream nextPutAll: object key].
	object printOn: stream.