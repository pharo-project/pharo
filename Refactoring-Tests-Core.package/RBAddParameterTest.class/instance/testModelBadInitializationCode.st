failure tests
testModelBadInitializationCode
	| refactoring |
	model removeClassNamed: #RBRefactoring.
	refactoring := RBAddParameterRefactoring 
		model: model
		addParameterToMethod: #name1
		in: RBLintRuleTest
		newSelector: #name1:
		initializer: 'AddParameterRefactoring new'.
	self shouldFail: refactoring