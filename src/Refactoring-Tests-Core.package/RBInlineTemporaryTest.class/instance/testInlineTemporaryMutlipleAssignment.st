tests
testInlineTemporaryMutlipleAssignment
	self shouldFail: (RBInlineTemporaryRefactoring 
			inline: (self 
					convertInterval: (60 to: 83)
					for: (RBRefactoryTestDataApp sourceCodeAt: #moveDefinition))
			from: #moveDefinition
			in: RBRefactoryTestDataApp)