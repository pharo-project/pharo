code generation
emitCodeForEffect: stack encoder: encoder

	self emitCodeForValue: stack encoder: encoder.
	encoder genPop.
	stack pop: 1