failure tests
testExistingName
	self shouldFail: (RBRenameClassRefactoring rename: self class to: #Object)