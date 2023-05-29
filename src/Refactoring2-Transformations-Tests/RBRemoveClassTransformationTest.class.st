Class {
	#name : #RBRemoveClassTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #accessing }
RBRemoveClassTransformationTest class >> defaultTimeLimit [
	^20 seconds
]

{ #category : #tests }
RBRemoveClassTransformationTest >> testRefactoring [
	| refactoring |
	refactoring := (RBRemoveClassTransformation className: #RBFooDummyLintRuleTest1)
	asRefactoring.

	self should:
	[ refactoring transform ]
		raise: RBRefactoringError.

	self assert: (refactoring model classNamed: #RBFooDummyLintRuleTest1) isNotNil.

]
