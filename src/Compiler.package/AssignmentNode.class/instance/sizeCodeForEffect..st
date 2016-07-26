code generation
sizeCodeForEffect: encoder

	^(variable sizeCodeForLoad: encoder)
	+ (value sizeCodeForValue: encoder)
	+ (variable sizeCodeForStorePop: encoder)