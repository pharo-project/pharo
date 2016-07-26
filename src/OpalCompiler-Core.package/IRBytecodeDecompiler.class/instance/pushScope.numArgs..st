scope
pushScope: copiedValues numArgs: numArgs
	|scope |
	scope := IRBytecodeScope new numArgs: numArgs.
	scopeStack push: scope.
	scope copiedValues: copiedValues.