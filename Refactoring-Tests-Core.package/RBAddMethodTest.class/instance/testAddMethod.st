tests
testAddMethod
	| refactoring |
	refactoring := RBAddMethodRefactoring 
		addMethod: 'printString1 ^super printString'
		toClass: RBBasicLintRuleTest
		inProtocols: #(#accessing ).
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBBasicLintRuleTest) parseTreeFor: #printString1) = (RBParser parseMethod: 'printString1 ^super printString')