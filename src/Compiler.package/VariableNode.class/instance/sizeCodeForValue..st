code generation
sizeCodeForValue: encoder
	self reserve: encoder.
	encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral| "i.e. the pseudo-variables nil true & false"
			 ^encoder sizePushSpecialLiteral: specialLiteral].
	(code = LdSelf or: [code = LdSuper]) ifTrue:
		[^encoder sizePushReceiver].
	code = LdThisContext ifTrue:
		[^encoder sizePushThisContext].
	self flag: 'probably superfluous'.
	^encoder sizePushInstVar: index