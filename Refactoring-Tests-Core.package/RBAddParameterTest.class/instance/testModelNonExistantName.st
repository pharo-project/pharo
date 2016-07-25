failure tests
testModelNonExistantName
	| refactoring |
	(model classNamed: #RBLintRuleTest) removeMethod: #name.
	refactoring := RBAddParameterRefactoring 
		model: model
		addParameterToMethod: #name
		in: RBLintRuleTest
		newSelector: #nameNew:
		initializer: 'nil'.
	self shouldFail: refactoring