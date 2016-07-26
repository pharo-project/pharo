failure tests
testBadName
	self shouldFail: (RBExtractToTemporaryRefactoring 
			extract: (self 
					convertInterval: (14 to: 23)
					for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
			to: 'a sdf'
			from: #testMethod
			in: RBRefactoryTestDataApp)