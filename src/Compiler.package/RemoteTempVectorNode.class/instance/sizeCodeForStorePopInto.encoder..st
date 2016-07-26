code generation (closures)
sizeCodeForStorePopInto: aTempVariableNode encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^encoder sizeStorePopRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1 inVectorAt: index].
	^(self sizeCodeForStoreInto: aTempVariableNode encoder: encoder)
	+ encoder sizePop