Class {
	#name : #RBMoveInstVarToClassTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core-Refactorings'
}

{ #category : #'failure tests' }
RBMoveInstVarToClassTest >> testMoveInstVarToClassAlreadyDefined [
	self
		shouldFail:
			(RBMoveInstVarToClassRefactoring
				model: model
				variable: 'result'
				class: RBBasicLintRuleTestData
				oldClass: RBFooLintRuleTestData)
]
