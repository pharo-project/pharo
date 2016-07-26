code generation
emitCodeForValue: stack encoder: encoder
	stack push: 1.
	(encoder
		if: code
		isSpecialLiteralForPush:
			[:specialLiteral|
			 encoder genPushSpecialLiteral: specialLiteral])
		ifFalse:
			[encoder genPushLiteral: index]