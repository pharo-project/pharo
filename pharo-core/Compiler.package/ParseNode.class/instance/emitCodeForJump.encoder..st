code generation
emitCodeForJump: dist encoder: encoder

	dist = 0 ifFalse: [encoder genJump: dist]