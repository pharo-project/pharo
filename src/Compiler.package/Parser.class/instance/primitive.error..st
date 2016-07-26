primitives
primitive: anIntegerOrString error: errorCodeVariableOrNil
	"Create indexed primitive with optional error code."
	
	<primitive>
	^anIntegerOrString isInteger
		ifTrue:
			[errorCodeVariableOrNil ifNotNil:
				[encoder floatTemp: (encoder bindTemp: errorCodeVariableOrNil) nowHasDef].
			 anIntegerOrString]
		ifFalse:
			[anIntegerOrString isString
				ifTrue: [self primitive: anIntegerOrString module: nil error: errorCodeVariableOrNil]
				ifFalse: [self expected: 'Indexed primitive']]