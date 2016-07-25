code generation
emitCodeForValue: stack encoder: encoder

	expr emitCodeForReturn: stack encoder: encoder.
	pc := encoder methodStreamPosition