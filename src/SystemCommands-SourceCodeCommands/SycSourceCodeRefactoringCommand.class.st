Class {
	#name : #SycSourceCodeRefactoringCommand,
	#superclass : #SycSourceCodeCommand,
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #testing }
SycSourceCodeRefactoringCommand class >> isAbstract [

	^ self == SycSourceCodeRefactoringCommand
]

{ #category : #testing }
SycSourceCodeRefactoringCommand >> isComplexRefactoring [
	^ true
]
