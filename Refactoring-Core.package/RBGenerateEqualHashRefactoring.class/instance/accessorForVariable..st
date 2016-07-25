transforming
accessorForVariable: aString
	| refactoring |
	refactoring := RBCreateAccessorsForVariableRefactoring
		model: self model
		variable: aString
		class: self theClass
		classVariable: false.
	refactoring createGetterAccessor.
	^ refactoring getterMethod