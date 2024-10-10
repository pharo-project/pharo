"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'SycRefactoringPreviewMockCommand',
	#superclass : 'Object',
	#category : 'SystemCommands-RefactoringSupport-Tests',
	#package : 'SystemCommands-RefactoringSupport-Tests'
}

{ #category : 'converting' }
SycRefactoringPreviewMockCommand >> asRefactorings [
	^ { SycMockRBRefactoring new  }
]

{ #category : 'converting' }
SycRefactoringPreviewMockCommand >> asRefactoringsFrom: aRBNamespace [ 

	^ { SycMockRBRefactoring new  }
]
