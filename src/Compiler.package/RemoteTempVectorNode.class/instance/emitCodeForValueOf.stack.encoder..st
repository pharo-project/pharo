code generation (closures)
emitCodeForValueOf: aTempVariableNode stack: stack encoder: encoder
	encoder supportsClosureOpcodes
		ifTrue:
			[encoder
				genPushRemoteTemp: (remoteTemps indexOf: aTempVariableNode) - 1
				inVectorAt: index.
			 stack push: 1]
		ifFalse:
			[self emitCodeForLoadFor: aTempVariableNode stack: stack encoder: encoder.
			 readNode
				emitCode: stack
				args: 1
				encoder: encoder
				super: false]