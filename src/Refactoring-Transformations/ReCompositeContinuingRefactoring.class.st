"
I represent a sequence of refactorings but I will skip refactorings failing and execute as much as possible.
"
Class {
	#name : 'ReCompositeContinuingRefactoring',
	#superclass : 'ReCompositeRefactoring',
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'preconditions' }
ReCompositeContinuingRefactoring >> privateTransform [

	refactorings do: [ :each | [ each generateChanges] on: RBRefactoringError do: [ :ex | ] ]
]
