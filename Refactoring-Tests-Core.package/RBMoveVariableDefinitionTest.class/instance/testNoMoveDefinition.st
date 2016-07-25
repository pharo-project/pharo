tests
testNoMoveDefinition
	self shouldFail: (RBMoveVariableDefinitionRefactoring 
			bindTight: (self 
					convertInterval: (21 to: 24)
					for: (RBRefactoryTestDataApp sourceCodeAt: #moveDefinition))
			in: RBRefactoryTestDataApp
			selector: #noMoveDefinition)