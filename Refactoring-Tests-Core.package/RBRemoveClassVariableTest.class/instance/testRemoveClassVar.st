tests
testRemoveClassVar
	| refactoring class |
	refactoring := RBRemoveClassVariableRefactoring 
		variable: 'Foo1'
		class: RBLintRuleTest.
	class := refactoring model classNamed: #RBLintRuleTest.
	self assert: (class definesClassVariable: 'Foo1').
	self executeRefactoring: refactoring.
	self deny: (class definesClassVariable: 'Foo1')