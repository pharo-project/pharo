"
I represent a sequence of refactorings but I will skip refactorings failing and execute as much as possible.
"
Class {
	#name : 'ReCompositeContinuingRefactoring',
	#superclass : 'ReCompositeRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
ReCompositeContinuingRefactoring >> privateTransform [

	refactorings do: [ :each | [ each generateChanges] on: RBRefactoringError do: [ :ex | ] ]
]
