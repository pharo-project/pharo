failure tests
testBadInterval
	self shouldFail: (RBRenameTemporaryRefactoring 
			renameTemporaryFrom: (self 
					convertInterval: (14 to: 17)
					for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
			to: 'asdf'
			in: RBRefactoryTestDataApp
			selector: #testMethod)