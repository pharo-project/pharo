failure tests
testPrimitive
	self shouldFail: (RBInlineMethodRefactoring 
			inline: (self 
					convertInterval: (14 to: 23)
					for: (RBRefactoryTestDataApp sourceCodeAt: #testMethod))
			inMethod: #testMethod
			forClass: RBRefactoryTestDataApp)