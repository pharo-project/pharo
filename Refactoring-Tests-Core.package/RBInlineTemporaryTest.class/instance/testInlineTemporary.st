tests
testInlineTemporary
	| refactoring |
	refactoring := RBInlineTemporaryRefactoring 
		inline: (self 
				convertInterval: (24 to: 72)
				for: (RBRefactoryTestDataApp sourceCodeAt: #inlineMethod))
		from: #inlineMethod
		in: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineMethod) = (RBParser parseMethod: 'inlineMethod
										^self
													foo;
													inlineMethod;
													bar')