code generation
emitCodeForReturn: stack encoder: encoder
	encoder
		if: code
		isSpecialLiteralForReturn:
			[:specialLiteral|
			"short returns"
			 encoder genReturnSpecialLiteral: specialLiteral.
			 stack push: 1 "doesnt seem right".
			 ^self].
	(self code = LdSelf or: [self code = LdSuper]) ifTrue: 
		["short returns"
		 encoder genReturnReceiver.
		 stack push: 1 "doesnt seem right".
		 ^self].
	super emitCodeForReturn: stack encoder: encoder