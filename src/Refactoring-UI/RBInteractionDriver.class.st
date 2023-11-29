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
		'refactoring',
		'previewPresenterClass'
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
RBInteractionDriver >> application [
	"For now use the singleton. Normally the application should be passed from Calypso and the command"
	
	^ StPharoApplication current

]

{ #category : 'execution' }
RBInteractionDriver >> applyChanges [

	self openPreviewWithChanges: self changes

]

{ #category : 'execution' }
RBInteractionDriver >> changes [

	^ self subclassResponsibility 

]

{ #category : 'execution' }
RBInteractionDriver >> furtherActionFor: aReport [
	
	aReport browse
]

{ #category : 'execution' }
RBInteractionDriver >> openPreviewWithChanges: changes [
	
	^ (self previewPresenterClass
		 changes: changes
		 scopes: scopes)
		application: self application;
		refactoring: self refactoring;
		openModal
]

{ #category : 'configuration' }
RBInteractionDriver >> previewPresenterClass [

	^ previewPresenterClass ifNil: [ previewPresenterClass := StRefactoringPreviewPresenter ] 
	
	
]

{ #category : 'configuration' }
RBInteractionDriver >> previewPresenterClass: aClass [

	 previewPresenterClass := aClass
	
	
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
