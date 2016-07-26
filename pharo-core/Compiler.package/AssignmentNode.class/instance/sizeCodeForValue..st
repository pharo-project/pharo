code generation
sizeCodeForValue: encoder

	^(variable sizeCodeForLoad: encoder)
	+ (value sizeCodeForValue: encoder)
	+ (variable sizeCodeForStore: encoder)