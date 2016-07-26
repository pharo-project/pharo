tests
testInlineTemporaryReadBeforeWritten
	self shouldFail: (RBInlineTemporaryRefactoring 
			inline: (self 
					convertInterval: (48 to: 56)
					for: (RBRefactoryTestDataApp sourceCodeAt: #inlineTemporary))
			from: #inlineTemporary
			in: RBRefactoryTestDataApp)