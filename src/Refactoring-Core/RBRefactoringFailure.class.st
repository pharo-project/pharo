"
This error is signalled whenever there is a non recoverable error during refactorings.
"
Class {
	#name : #RBRefactoringFailure,
	#superclass : #RBRefactoringError,
	#category : 'Refactoring-Core-Support'
}

{ #category : #testing }
RBRefactoringFailure >> isResumable [
	^ false
]
