tests
testPushDownClassVariable
	| refactoring |
	refactoring := RBPushDownClassVariableRefactoring 
		variable: #Foo1
		class: RBLintRuleTest.
	self assert: ((refactoring model classNamed: #RBLintRuleTest) directlyDefinesClassVariable: #Foo1).
	self executeRefactoring: refactoring.
	(refactoring model classNamed: #RBLintRuleTest) withAllSubclasses do: [ :each | self deny: (each directlyDefinesClassVariable: #Foo1) ]