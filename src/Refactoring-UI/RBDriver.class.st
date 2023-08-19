"
I represent a bridge between refactoring core and UI. I am abstract class and you should use my subclasses to invoke specific refactorings.

I am a driver object responsible for invoking refactorings. I am responsible for:
- gathering information from user that are needed to perform a refactoring
- checking preconditions and raising an error if they are not fulfilled
- checking braking changes and warning user if they exist
- and finally, invoking refactoring


"
Class {
	#name : #RBDriver,
	#superclass : #Object,
	#instVars : [
		'model',
		'scopes'
	],
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #testing }
RBDriver class >> isAbstract [

	^ self == RBDriver
]

{ #category : #execution }
RBDriver >> furtherActionFor: aReport [
	
	aReport browse
]

{ #category : #execution }
RBDriver >> openPreviewWithChanges: changes [
	
	^ (StRefactoringPreviewPresenter
		 changes: changes
		 scopes: scopes)
		refactoring: self refactoring;
		openModal
]

{ #category : #utilities }
RBDriver >> refactoringScopeOn: aScope [

	^ RBClassModelFactory rbNamespace onEnvironment: aScope asRBEnvironment
]

{ #category : #execution }
RBDriver >> runRefactoring [

	self subclassResponsibility
]
