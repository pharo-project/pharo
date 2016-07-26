code generation
sizeCodeForValue: encoder
	self reserve: encoder.
	(encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral|
			 ^encoder sizePushSpecialLiteral: specialLiteral])
		ifFalse:
			[^encoder sizePushLiteral: index]