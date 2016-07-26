failure tests
testBadInterval
	self
		shouldFail: (RBExtractMethodToComponentRefactoring 
				extract: (self 
						convertInterval: (24 to: 30)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
				from: #testMethod
				in: RBRefactoryTestDataApp);
		shouldFail: (RBExtractMethodToComponentRefactoring 
				extract: (self 
						convertInterval: (80 to: 147)
						for: (RBBasicLintRuleTest class sourceCodeAt: #subclassOf:overrides:))
				from: #subclassOf:overrides:
				in: RBBasicLintRuleTest class)