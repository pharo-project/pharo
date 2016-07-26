tests
testInlineMethod3
	| refactoring |
	refactoring := RBInlineMethodRefactoring 
		inline: (self 
				convertInterval: (58 to: 73)
				for: (RBRefactoryTestDataApp sourceCodeAt: #caller2))
		inMethod: #caller2
		forClass: RBRefactoryTestDataApp.
	self 
		setupInlineExpressionFor: refactoring
		toReturn: false.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #caller2) = (RBParser parseMethod: 'caller2
								^(1 to: 10) inject: 1 into: [:sum :each | sum * ((1 to: 10) inject: each into: [:sum1 :each1 | sum1 + each1])]	')