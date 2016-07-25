failure tests
testNonExistantSelector
	self shouldFail: (RBExtractToTemporaryRefactoring 
			extract: (14 to: 23)
			to: 'asdf'
			from: #checkClass1:
			in: RBRefactoryTestDataApp)