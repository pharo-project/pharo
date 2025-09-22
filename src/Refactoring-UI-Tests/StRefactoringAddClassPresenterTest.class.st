"
A StRefactoringAddClassPresenterTest is a test class for testing the behavior of StRefactoringAddClassPresenter
"
Class {
	#name : 'StRefactoringAddClassPresenterTest',
	#superclass : 'StRequestClassPresenterTest',
	#category : 'Refactoring-UI-Tests-UI',
	#package : 'Refactoring-UI-Tests',
	#tag : 'UI'
}

{ #category : 'accessing' }
StRefactoringAddClassPresenterTest >> newPresenter [

	^ StRefactoringAddClassPresenter on: (ReAddSubclassDriver basicNew
			   superclass: Object;
			   yourself)
]
