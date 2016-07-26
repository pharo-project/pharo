code generation
emitCodeForEffect: stack encoder: encoder

	variable emitCodeForLoad: stack encoder: encoder.
	value emitCodeForValue: stack encoder: encoder.
	pc := encoder methodStreamPosition + 1. "debug pc is first byte of the store, i.e. the next byte".
	variable emitCodeForStorePop: stack encoder: encoder