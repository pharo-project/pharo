transforming
abstractVariableReferences
	| refactoring |
	refactoring := RBAbstractVariablesRefactoring 
				model: self model
				abstractVariablesIn: inlineParseTree
				from: inlineClass
				toAll: (Array with: class).
	self performComponentRefactoring: refactoring.
	inlineParseTree := refactoring parseTree