code generation
sizeCodeForValue: encoder
	self reserve: encoder.
	^encoder sizePushLiteralVar: index