Class {
	#name : #SycRefactoringPreviewMockCommand,
	#superclass : #Object,
	#category : #'SystemCommands-RefactoringSupport-Test'
}

{ #category : #converting }
SycRefactoringPreviewMockCommand >> asRefactorings [ 
	^ { SycMockRBRefactoring new  } 
]
