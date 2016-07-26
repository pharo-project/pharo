code generation
sizeCodeForStorePop: encoder
	self reserve: encoder.
	^encoder sizeStorePopLiteralVar: index