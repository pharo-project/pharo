failure tests
testNonExistantSelector
	self shouldFail: (RBExtractMethodRefactoring 
			extract: (10 to: 20)
			from: #checkClass1:
			in: RBBasicLintRuleTest)