failure tests
testModelAlreadyExistingName
	| refactoring |
	refactoring := RBAddClassVariableRefactoring 
				model: model
				variable: #ClassVarName2
				class: (model classNamed: #Bar).
	self shouldFail: refactoring