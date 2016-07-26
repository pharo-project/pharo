failure tests
testInlineMethodFromComponentFailure
	self shouldFail: (RBInlineMethodFromComponentRefactoring 
			inline: (self 
					convertInterval: (50 to: 64)
					for: (RBRefactoryTestDataApp sourceCodeAt: #inlineFailed))
			inMethod: #inlineFailed
			forClass: RBRefactoryTestDataApp)