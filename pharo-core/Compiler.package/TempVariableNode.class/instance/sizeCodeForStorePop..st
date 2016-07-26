code generation
sizeCodeForStorePop: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForStorePopInto: self encoder: encoder].
	self reserve: encoder.
	^encoder sizeStorePopTemp: index