code generation
emitCodeForValue: stack encoder: encoder
	stack push: 1.
	^encoder genPushInstVarLong: index