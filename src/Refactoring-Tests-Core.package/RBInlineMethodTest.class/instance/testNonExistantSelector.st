failure tests
testNonExistantSelector
	self shouldFail: (RBInlineMethodRefactoring 
			inline: (14 to: 17)
			inMethod: #checkClass1:
			forClass: RBRefactoryTestDataApp)