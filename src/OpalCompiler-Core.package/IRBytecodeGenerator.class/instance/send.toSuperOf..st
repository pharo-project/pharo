instructions
send: selector toSuperOf: behavior

	(encoder class = EncoderForSistaV1 and: [ inBlock ])
		ifTrue: [ 
			| index nArgs |
			behavior isTrait ifTrue: [ 
				"Trait methods are copied to the users and only the last literal is updated. 
				 For directed super send the literal of the super send should be updated too."
				self error: 'not supported' ].
			nArgs := selector numArgs.
			stack pop: nArgs.
			encoder genPushLiteralVar: (self literalIndexOf: behavior binding).
			index := self literalIndexOf: selector.
			encoder genSendDirectedSuper: index numArgs: nArgs ]
		ifFalse: [ 
			| index nArgs |
			nArgs := selector numArgs.
			stack pop: nArgs.
			self addLastLiteral: behavior binding.  
			index := self literalIndexOf: selector.
			encoder genSendSuper: index numArgs: nArgs ]
