failure tests
testMetaClassFailure
	self 
		shouldFail: (RBRenameClassRefactoring rename: self class class to: #Foo)