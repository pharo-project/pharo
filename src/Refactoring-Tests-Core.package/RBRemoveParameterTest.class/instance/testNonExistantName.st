failure tests
testNonExistantName
	self
		shouldFail: (RBRemoveParameterRefactoring 
				removeParameter: 'asdf'
				in: RBBasicLintRuleTest
				selector: #checkClass:);
		shouldFail: (RBRemoveParameterRefactoring 
				removeParameter: 'aSmalllintContext'
				in: RBBasicLintRuleTest
				selector: #checkClass1:)