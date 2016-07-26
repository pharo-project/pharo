tests
testInlineMethod4
	| refactoring |
	refactoring := RBInlineMethodRefactoring 
		inline: (self 
				convertInterval: (31 to: 112)
				for: (RBRefactoryTestDataApp sourceCodeAt: #inlineJunk))
		inMethod: #inlineJunk
		forClass: RBRefactoryTestDataApp.
	self 
		setupInlineExpressionFor: refactoring
		toReturn: false.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineJunk) = (RBParser parseMethod: 'inlineJunk
										| asdf bar1 baz1 asdf1 |
										bar1 := 
												[:each | 
												| temp |
												temp := each.
												temp , temp] value: self.
										baz1 := bar1 + bar1.
										asdf1 := baz1 + bar1.
										asdf := asdf1.
										^asdf foo: 
												[:bar | 
												| baz |
												baz := bar.
												baz * baz]')