transforming
addNewAccessors
	| refactoring |
	refactoring := RBCreateAccessorsForVariableRefactoring
		model: self
		variable: newName asString
		class: class
		classVariable: false.
	refactoring
		createGetterAccessor;
		createSetterAccessor