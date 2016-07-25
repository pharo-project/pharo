tests
testRemoveClass
	| refactoring |
	refactoring := RBRemoveClassRefactoring classNames: (Array with: ('RBFoo' , 'LintRuleTest1') asSymbol).
	[ self executeRefactoring: refactoring ]
		on: RBRefactoringError
		do: [ :error | self resumeOnCannotRemoveTopLevelClassError: error ].
	self assert: (refactoring model classNamed: ('RBFoo' , 'LintRuleTest1') asSymbol) isNil.
	self
		assert:
			(refactoring model classNamed: #RBTransformationRuleTest1) superclass = (refactoring model classNamed: #RBLintRuleTest)