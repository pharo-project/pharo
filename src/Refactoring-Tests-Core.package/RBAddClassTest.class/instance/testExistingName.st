failure tests
testExistingName
	self shouldFail: (RBAddClassRefactoring 
				addClass: #Object
				superclass: self class
				subclasses: #()
				category: #'Refactory-Testing')