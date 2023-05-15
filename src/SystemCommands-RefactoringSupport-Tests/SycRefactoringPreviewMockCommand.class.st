Class {
	#name : #SycRefactoringPreviewMockCommand,
	#superclass : #Object,
	#category : #'SystemCommands-RefactoringSupport-Tests'
}

{ #category : #converting }
SycRefactoringPreviewMockCommand >> asRefactorings [
	^ { SycMockRBRefactoring new  }
]
