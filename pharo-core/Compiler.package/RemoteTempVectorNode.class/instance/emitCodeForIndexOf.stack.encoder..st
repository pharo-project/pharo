code generation (closures)
emitCodeForIndexOf: aTempVariableNode stack: stack encoder: encoder
	[encoder supportsClosureOpcodes not] assert.
	(encoder encodeLiteral: (remoteTemps indexOf: aTempVariableNode))
		emitCodeForValue: stack encoder: encoder