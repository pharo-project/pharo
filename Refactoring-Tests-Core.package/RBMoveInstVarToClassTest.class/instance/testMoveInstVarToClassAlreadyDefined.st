failure tests
testMoveInstVarToClassAlreadyDefined
	self
		shouldFail:
			(RBMoveInstVarToClassRefactoring
				model: model
				variable: 'result'
				class: RBBasicLintRuleTest
				oldClass: RBFooLintRuleTest)