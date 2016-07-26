tests
testInlineMethod1
	| refactoring |
	refactoring := RBInlineMethodRefactoring 
		inline: (self 
				convertInterval: (39 to: 84)
				for: (RBRefactoryTestDataApp sourceCodeAt: #caller))
		inMethod: #caller
		forClass: RBRefactoryTestDataApp.
	self 
		setupInlineExpressionFor: refactoring
		toReturn: false.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #caller) = (RBParser parseMethod: 'caller 
									| anObject anObject1 | 
									anObject := 5.
									anObject1 := anObject + 1.
									Transcript
										show: anObject1 printString;
										cr.
									^anObject')