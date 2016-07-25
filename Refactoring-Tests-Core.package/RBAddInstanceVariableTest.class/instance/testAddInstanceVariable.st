tests
testAddInstanceVariable
	| refactoring |
	refactoring := RBAddInstanceVariableRefactoring 
		variable: 'asdf'
		class: RBTransformationRuleTest.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBTransformationRuleTest) directlyDefinesInstanceVariable: 'asdf')