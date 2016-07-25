failure tests
testInlineBlockFailure
	self shouldFail: (RBInlineParameterRefactoring 
			inlineParameter: 'aBlock'
			in: RBRefactoryTestDataApp
			selector: ('inline' , 'Foo:') asSymbol)