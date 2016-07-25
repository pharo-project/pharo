instructions
send: selector toSuperOf: behavior

	| index nArgs |
	nArgs := selector numArgs.
	stack pop: nArgs.

	self addLastLiteral: behavior binding.  
	index := self literalIndexOf: selector.
	encoder genSendSuper: index numArgs: nArgs
