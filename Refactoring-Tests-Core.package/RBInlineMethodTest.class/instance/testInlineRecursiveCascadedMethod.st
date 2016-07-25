tests
testInlineRecursiveCascadedMethod
	| refactoring |
	refactoring := RBInlineMethodRefactoring 
		inline: (self 
				convertInterval: (33 to: 62)
				for: (RBRefactoryTestDataApp sourceCodeAt: #inlineMethod))
		inMethod: #inlineMethod
		forClass: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineMethod) = (RBParser parseMethod: 'inlineMethod
									| temp temp1 |
									self foo.
									temp1 := self foo; inlineMethod; bar.
									temp := self bar.
									^temp')