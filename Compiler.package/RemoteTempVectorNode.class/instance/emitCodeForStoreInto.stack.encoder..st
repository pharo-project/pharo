code generation (closures)
emitCodeForStoreInto: aTempVariableNode stack: stack encoder: encoder
	encoder supportsClosureOpcodes
		ifTrue:
			[encoder
				genStoreRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
				inVectorAt: index]
		ifFalse:
			[writeNode
				emitCode: stack
				args: 2
				encoder: encoder
				super: false]