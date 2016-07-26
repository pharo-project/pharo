failure tests
testMetaClassFailure
	self shouldFail: (RBAddClassRefactoring 
				addClass: #Foo
				superclass: self class class
				subclasses: #()
				category: #'Refactory-Testing')