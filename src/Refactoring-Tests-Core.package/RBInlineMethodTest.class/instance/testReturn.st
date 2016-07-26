failure tests
testReturn
	self shouldFail: (RBInlineMethodRefactoring 
			inline: (self 
					convertInterval: (418 to: 485)
					for: (RBBasicLintRuleTest class sourceCodeAt: #utilityMethods))
			inMethod: #utilityMethods
			forClass: RBBasicLintRuleTest class)