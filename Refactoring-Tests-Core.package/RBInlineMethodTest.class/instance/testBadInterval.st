failure tests
testBadInterval
	self
		shouldFail: (RBInlineMethodRefactoring 
				inline: (self 
						convertInterval: (13 to: 23)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
				inMethod: #testMethod
				forClass: RBRefactoryTestDataApp);
		shouldFail: (RBInlineMethodRefactoring 
				inline: (self 
						convertInterval: (14 to: 17)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
				inMethod: #testMethod
				forClass: RBRefactoryTestDataApp);
		shouldFail: (RBInlineMethodRefactoring 
				inline: (self 
						convertInterval: (24 to: 30)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
				inMethod: #testMethod
				forClass: RBRefactoryTestDataApp);
		shouldFail: (RBInlineMethodRefactoring 
				inline: (self 
						convertInterval: (1 to: 30)
						for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
				inMethod: #testMethod
				forClass: RBRefactoryTestDataApp)