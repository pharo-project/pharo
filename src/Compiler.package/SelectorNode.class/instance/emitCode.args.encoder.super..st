code generation
emitCode: stack args: nArgs encoder: encoder super: supered
	stack pop: nArgs.
	^supered
		ifTrue:
			[encoder genSendSuper: index numArgs: nArgs]
		ifFalse:
			[encoder
				genSend: (code < Send ifTrue: [code negated] ifFalse: [index])
				numArgs: nArgs]