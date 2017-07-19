accessing
bytecodeBackend
	^ BytecodeBackend ifNil: [ self initialize. BytecodeBackend ]