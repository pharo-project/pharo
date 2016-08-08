tests
testRemoveEmptySuperclass
	| refactoring |
	refactoring := RBRemoveClassRefactoring classNames: (Array with: ('RBFoo' , 'LintRuleTest1') asSymbol).
	self executeRefactoring: refactoring.
	self assert: (refactoring model classNamed: ('RBFoo' , 'LintRuleTest1') asSymbol) isNil.
	self assert: (refactoring model classNamed: #RBTransformationRuleTest1) superclass = (refactoring model classNamed: #RBLintRuleTest)