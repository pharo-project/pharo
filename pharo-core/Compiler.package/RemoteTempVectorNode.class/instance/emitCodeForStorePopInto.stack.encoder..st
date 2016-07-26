code generation (closures)
emitCodeForStorePopInto: aTempVariableNode stack: stack encoder: encoder
	encoder supportsClosureOpcodes
		ifTrue:
			[encoder
				genStorePopRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
				inVectorAt: index]
		ifFalse:
			[self emitCodeForStoreInto: aTempVariableNode stack: stack encoder: encoder.
			 encoder genPop].
	stack pop: 1