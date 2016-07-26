instructions
send: selector
	| nArgs |
	nArgs := selector numArgs.
	stack pop: nArgs.
	(self encoderClass specialSelectors includes: selector)
		ifTrue: [ ^ encoder genSendSpecial: (self encoderClass specialSelectors indexOf: selector) numArgs: nArgs ].
	encoder genSend: (self literalIndexOf: selector) numArgs: nArgs