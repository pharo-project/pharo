failure tests
testExtractFailure
	self
		shouldFail: (RBExtractMethodToComponentRefactoring 
				extract: (self 
						convertInterval: (80 to: 269)
						for: (RBBasicLintRuleTest class sourceCodeAt: #subclassOf:overrides:))
				from: #subclassOf:overrides:
				in: RBBasicLintRuleTest class);
		shouldFail: (RBExtractMethodToComponentRefactoring 
				extract: (self 
						convertInterval: (53 to: 56)
						for: (RBBasicLintRuleTest class sourceCodeAt: #subclassOf:overrides:))
				from: #subclassOf:overrides:
				in: RBBasicLintRuleTest class);
		shouldFail: (RBExtractMethodToComponentRefactoring 
				extract: (self 
						convertInterval: (77 to: 222)
						for: (RBBasicLintRuleTest class sourceCodeAt: #subclassResponsibilityNotDefined))
				from: #subclassResponsibilityNotDefined
				in: RBBasicLintRuleTest class)