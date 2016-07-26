failure tests
testNonExistantSelector
	self shouldFail: (RBExtractMethodToComponentRefactoring 
			extract: (10 to: 20)
			from: #checkClass1:
			in: RBBasicLintRuleTest)