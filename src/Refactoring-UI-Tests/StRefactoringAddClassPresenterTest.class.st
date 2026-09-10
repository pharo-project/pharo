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

	^ StRefactoringAddClassPresenter on: (ReInsertSubclassDriver basicNew
			   superclass: Object;
			   yourself)
]

{ #category : 'tests' }
StRefactoringAddClassPresenterTest >> testClassNameInputIsRecordedInDriver [
	| driver |
	
	driver := presenter instVarNamed: 'driver'.
	(presenter instVarNamed: 'textInput') text: 'Object'.
	self assert: driver subclass equals: 'Object'
]

{ #category : 'tests' }
StRefactoringAddClassPresenterTest >> testCommentTemplateActivationFillsComment [

	| checkbox comment |
	checkbox := presenter instVarNamed: 'useTemplateCheckPresenter'.
	comment := presenter instVarNamed: 'commentPresenter'.
	self assert: comment text isEmpty.
	checkbox state: true.

	self deny: comment text isEmpty.
	self assert: comment text equals: Object classCommentBlank
]
