accessing
promptOnRefactoring

	^ PromptOnRefactoring ifNil: [ PromptOnRefactoring := true ]