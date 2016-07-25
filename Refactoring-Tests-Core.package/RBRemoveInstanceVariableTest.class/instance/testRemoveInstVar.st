tests
testRemoveInstVar
	| refactoring class |
	refactoring := RBRemoveInstanceVariableRefactoring 
		variable: 'foo1'
		class: RBLintRuleTest.
	class := refactoring model classNamed: #RBLintRuleTest.
	self assert: (class definesInstanceVariable: 'foo1').
	self executeRefactoring: refactoring.
	self deny: (class definesInstanceVariable: 'foo1')