code generation (closures)
emitCodeForLoadFor: aTempVariableNode stack: stack encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^self].
	"Need to generate the first half of
		tempVector at: index put: expr
	 i.e. the push of tempVector and index."
	super emitCodeForValue: stack encoder: encoder.
	self emitCodeForIndexOf: aTempVariableNode stack: stack encoder: encoder