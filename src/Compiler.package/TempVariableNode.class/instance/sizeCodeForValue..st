code generation
sizeCodeForValue: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode sizeCodeForValueOf: self encoder: encoder].
	self reserve: encoder.
	^encoder sizePushTemp: index