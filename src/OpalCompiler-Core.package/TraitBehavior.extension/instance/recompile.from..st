*OpalCompiler-Core
recompile: selector from: oldClass
	"Compile the method associated with selector in the receiver's method dictionary."

	| method methodNode |
	method := oldClass compiledMethodAt: selector.
	methodNode := self compiler
				source: (oldClass sourceCodeAt: selector);
				failBlock:  [^ self];
				translate.
	selector == methodNode selector ifFalse: [self error: 'selector changed!'].
	self basicAddSelector: selector withMethod: (methodNode generate: method trailer).
	"self notifyUsersOfChangedSelector: selector."
