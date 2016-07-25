tests
testPushDownInstanceVariable
	| refactoring |
	refactoring := RBPushDownInstanceVariableRefactoring 
		variable: 'foo1'
		class: RBLintRuleTest.
	self executeRefactoring: refactoring.
	(refactoring model classNamed: #RBLintRuleTest) subclasses do: [ :each | self assert: (each directlyDefinesInstanceVariable: 'foo1') ]