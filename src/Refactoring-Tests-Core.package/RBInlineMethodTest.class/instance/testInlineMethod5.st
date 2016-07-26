tests
testInlineMethod5
	| refactoring |
	refactoring := RBInlineMethodRefactoring 
		inline: (self 
				convertInterval: (53 to: 64)
				for: (RBRefactoryTestDataApp sourceCodeAt: #inlineLast))
		inMethod: #inlineLast
		forClass: RBRefactoryTestDataApp.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #inlineLast) = (RBParser parseMethod: 'inlineLast
									5 = 3 ifTrue: [^self caller] ifFalse: [^	(1 to: 10) inject: 1 into: [:sum :each | sum * (self foo: each)]]')