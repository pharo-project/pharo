failure tests
testMetaclass
	| refactoring |
	refactoring := RBAddClassVariableRefactoring 
		variable: #ClassVarName3
		class: (model classNamed: #Object) theMetaClass.
	refactoring model: model.
	self shouldFail: refactoring