code generation
emitCodeForReturn: stack encoder: encoder

	self emitCodeForValue: stack encoder: encoder.
	encoder genReturnTop