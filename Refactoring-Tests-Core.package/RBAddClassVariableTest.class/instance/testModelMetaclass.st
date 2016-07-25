failure tests
testModelMetaclass
	| refactoring |
	refactoring := RBAddClassVariableRefactoring 
		model: model
		variable: #ClassVarName3
		class: (model classNamed: #Bar) theMetaClass.
	self shouldFail: refactoring