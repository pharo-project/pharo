code generation (closures)
sizeCodeForLoadFor: aTempVariableNode encoder: encoder
	encoder supportsClosureOpcodes ifTrue:
		[^0].
	"Need to size the first half of
		tempVector at: index put: expr
	 i.e. the push of tempVector and index."
	^(super sizeCodeForValue: encoder)
	+ (self sizeCodeForIndexOf: aTempVariableNode encoder: encoder)