code generation (closures)
sizeCodeForIndexOf: aTempVariableNode encoder: encoder
	[encoder supportsClosureOpcodes not] assert.
	^(encoder encodeLiteral: (remoteTemps indexOf: aTempVariableNode)) sizeCodeForValue: encoder