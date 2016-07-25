tests
testInlineMethod2
	| refactoring |
	refactoring := RBInlineMethodRefactoring 
		inline: (self 
				convertInterval: (40 to: 120)
				for: (RBRefactoryTestDataApp sourceCodeAt: #caller1))
		inMethod: #caller1
		forClass: RBRefactoryTestDataApp.
	self 
		setupInlineExpressionFor: refactoring
		toReturn: false.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBRefactoryTestDataApp) parseTreeFor: #caller1) = (RBParser parseMethod: 'caller1 
								| anObject each1 anObject1 | 
								anObject := 5.
								anObject1 := anObject + 1.
								each1 := anObject1 printString.
								Transcript
									show: each1;
									cr.
								[:each | each printString. ^anObject] value: each1')