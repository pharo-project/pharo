tests
testRenameTestMethod1
	| refactoring class |
	refactoring := RBRenameMethodRefactoring 
		renameMethod: ('testMethod1') asSymbol
		in: RBRefactoryTestDataApp
		to: #testMethod2
		permutation: (1 to: 0).
	self executeRefactoring: refactoring.
	class := refactoring model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: #testMethod2) = (RBParser parseMethod: 'testMethod2
	^self testMethod2 , ([:each | each testMethod2] value: #(#(#testMethod2) 2 #testMethod2))').
	self assert: ((refactoring model classNamed: #RBBasicLintRuleTest) parseTreeFor: #classBlock:) = (RBParser parseMethod: 'classBlock: aBlock
	classBlock := aBlock testMethod2').
	self deny: (class directlyDefinesMethod: ('test' , 'Method1') asSymbol)