tests
testPullUpClassVariable
	| refactoring |
	refactoring := RBPullUpClassVariableRefactoring 
		variable: #RecursiveSelfRule
		class: RBLintRuleTest.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBLintRuleTest) directlyDefinesClassVariable: #RecursiveSelfRule).
	self deny: ((refactoring model classNamed: #RBTransformationRuleTest) directlyDefinesClassVariable: #RecursiveSelfRule)