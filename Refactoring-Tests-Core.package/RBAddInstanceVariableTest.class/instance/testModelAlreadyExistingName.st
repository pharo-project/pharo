failure tests
testModelAlreadyExistingName
	| refactoring |
	refactoring := RBAddInstanceVariableRefactoring 
				model: model
				variable: 'instVarName1'
				class: (model classNamed: #Bar).
	self shouldFail: refactoring