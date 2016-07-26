code generation
sizeCodeForLoad: encoder
	self reserve: encoder.
	^writeNode ifNil: [0] ifNotNil: [encoder sizePushLiteral: index]