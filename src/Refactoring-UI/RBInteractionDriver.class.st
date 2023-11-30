"
I represent a bridge between refactoring core and UI. I am abstract class and you should use my subclasses to invoke specific refactorings.

I am a driver object responsible for invoking refactorings. I am responsible for:
- gathering information from user that are needed to perform a refactoring
- checking preconditions and raising an error if they are not fulfilled
- checking braking changes and warning user if they exist
- and finally, invoking refactoring


"
Class {
	#name : 'RBInteractionDriver',
	#superclass : 'Object',
	#instVars : [
		'model',
		'scopes',
		'refactoring'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'testing' }
RBInteractionDriver class >> isAbstract [

	^ self == RBInteractionDriver
]

{ #category : 'execution' }
RBInteractionDriver >> applyChanges [
	"I'm a hook to actually execute the changes"
	
	refactoring execute 
]

{ #category : 'execution' }
RBInteractionDriver >> furtherActionFor: aReport [
	
	aReport browse
]

{ #category : 'accessing' }
RBInteractionDriver >> model: aRBBrowserEnvironment [ 
	model := aRBBrowserEnvironment
]

{ #category : 'execution' }
RBInteractionDriver >> openPreviewWithChanges: changes [
	
	^ (StRefactoringPreviewPresenter
		 changes: changes
		 scopes: scopes)
		refactoring: self refactoring;
		openModal
]

{ #category : 'resources' }
RBInteractionDriver >> refactoring [
	^ refactoring
]

{ #category : 'utilities' }
RBInteractionDriver >> refactoringScopeOn: aScope [

	^ RBNamespace onEnvironment: aScope asRBEnvironment
]

{ #category : 'execution' }
RBInteractionDriver >> runRefactoring [

	self subclassResponsibility
]
