option
promptOnRefactoring
	^ promptOnRefactoring ifNil: [ self class promptOnRefactoring ]