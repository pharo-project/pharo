Class {
	#name : #RBRefactoringError,
	#superclass : #Error,
	#category : #'Refactoring-Core-Support'
}

{ #category : #signalling }
RBRefactoringError class >> signal: aString with: aBlock [
	^ self new
		actionBlock: aBlock;
		signal: aString
]
