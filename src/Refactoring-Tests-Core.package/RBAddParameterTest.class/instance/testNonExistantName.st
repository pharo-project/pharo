failure tests
testNonExistantName
	self shouldFail: (RBAddParameterRefactoring 
			addParameterToMethod: #name1
			in: RBLintRuleTest
			newSelector: #name1:
			initializer: 'nil')