code generation (closures)
sizeCodeForStoreInto: aTempVariableNode encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^encoder sizeStoreRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1 inVectorAt: index].
	^writeNode sizeCode: encoder args: 2 super: false