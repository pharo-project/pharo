transforming
abstractVariablesRefactoring
	^RBAbstractVariablesRefactoring 
		model: self model
		abstractVariablesIn: parseTree
		from: class
		toAll: moveToClasses
		ignoring: variable