failure tests
testBadInterval
	self
		shouldFail: (RBExtractToTemporaryRefactoring 
				extract: (self 
						convertInterval: (24 to: 30)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
				to: 'asdf'
				from: #testMethod
				in: RBRefactoryTestDataApp);
		shouldFail: (RBExtractToTemporaryRefactoring 
				extract: (self 
						convertInterval: (14 to: 105)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod1))
				to: 'asdf'
				from: #testMethod1
				in: RBRefactoryTestDataApp);
		shouldFail: (RBExtractToTemporaryRefactoring 
				extract: (self 
						convertInterval: (61 to: 101)
						for: (RBRefactoryTestDataApp sourceCodeAt: #noMoveDefinition))
				to: 'asdf'
				from: #noMoveDefinition
				in: RBRefactoryTestDataApp)