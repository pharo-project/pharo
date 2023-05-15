Class {
	#name : #RBRemoveClassTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #accessing }
RBRemoveClassTransformationTest class >> defaultTimeLimit [
	^20 seconds
]

{ #category : #private }
RBRemoveClassTransformationTest >> resumeIfCannotRemove: error [

	^ error messageText = RBRemoveClassTransformation
		cannotRemoveTopLevelClassErrorMesssage
		ifTrue: [ error resume ]
]

{ #category : #tests }
RBRemoveClassTransformationTest >> testRefactoring [
	| refactoring |
	refactoring := (RBRemoveClassTransformation className: #RBFooDummyLintRuleTest1)
	asRefactoring.

	[ refactoring transform ]
		on: RBRefactoringError
		do: [ :error | self resumeIfCannotRemove: error ].

	self assert: (refactoring model classNamed: #RBFooDummyLintRuleTest1) isNil.
	self assert: (refactoring model classNamed: 'RBTransformationDummyRuleTest1' asSymbol) superclass equals: (refactoring model classNamed: #RBDummyLintRuleTest)
]
