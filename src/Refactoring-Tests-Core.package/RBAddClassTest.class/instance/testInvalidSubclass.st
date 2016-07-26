failure tests
testInvalidSubclass
	self shouldFail: (RBAddClassRefactoring 
			addClass: #Foo
			superclass: RBCompositeLintRuleTest
			subclasses: (Array with: RBBasicLintRuleTest)
			category: #'Refactory-Tesing')