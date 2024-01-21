"
I represent a sequence of refactorings but I will skip refactorings failing and execute as much as possible.
"
Class {
	#name : 'RBCompositeContinuingRefactoring',
	#superclass : 'ReCompositeRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBCompositeContinuingRefactoring >> privateTransform [

	refactorings do: [ :each | [ each generateChanges] on: RBRefactoringError do: [ :ex | ] ]
]
