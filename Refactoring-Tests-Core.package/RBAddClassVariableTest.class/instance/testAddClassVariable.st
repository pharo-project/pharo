tests
testAddClassVariable
	| refactoring |
	refactoring := RBAddClassVariableRefactoring 
		variable: 'Asdf'
		class: RBTransformationRuleTest.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBTransformationRuleTest) directlyDefinesClassVariable: #Asdf)