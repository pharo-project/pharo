failure tests
testModelMetaclassAlreadyExistingName
	| refactoring |
	(model metaclassNamed: #Foo) addInstanceVariable: 'instVarName1'.
	refactoring := RBAddInstanceVariableRefactoring 
				model: model
				variable: 'instVarName1'
				class: (model classNamed: #Bar).
	self shouldFail: refactoring