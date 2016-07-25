code generation (closures)
sizeCodeForValueOf: aTempVariableNode encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^encoder sizePushRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1 inVectorAt: index].
	^(self sizeCodeForValue: encoder)
	+ (self sizeCodeForIndexOf: aTempVariableNode encoder: encoder)
	+ (readNode sizeCode: encoder args: 1 super: false)