"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'MockSycRefactoringPreviewPresenter',
	#superclass : 'SycRefactoringPreviewPresenter',
	#category : 'SystemCommands-RefactoringSupport-Tests',
	#package : 'SystemCommands-RefactoringSupport-Tests'
}

{ #category : 'private' }
MockSycRefactoringPreviewPresenter >> buildDiffFor: aChange [
	"i override this method because i just want to test if the transmission use this method"
	^ 'useForTest'
]

{ #category : 'accessing' }
MockSycRefactoringPreviewPresenter >> selectedRefactorings [
	^ selectedRefactorings
]
