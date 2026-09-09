"
I'm an interaction class in charge of stirring the application or not of removing shared variables.
"
Class {
	#name : 'ReRemoveSharedVariablesDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'variables'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReRemoveSharedVariablesDriver class >> driverName [

	^ 'Remove shared variable(s)'
]

{ #category : 'accessing' }
ReRemoveSharedVariablesDriver class >> shouldPreviewChangesByDefault [

	^ false
]

{ #category : 'execution' }
ReRemoveSharedVariablesDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
	items add: (ReBrowseReferencedSharedVariableChoice new driver: self ; yourself).
	items add: (ReRemoveSharedVariableChoice new driver: self; yourself).
	items add: (ReRemoveSharedVariableAndShowChoice new driver: self; yourself).
	^ items
]

{ #category : 'execution' }
ReRemoveSharedVariablesDriver >> browseSharedVariableReferences [

	(Smalltalk tools toolNamed: #messageList) browse: refactoring violators
]

{ #category : 'configuration' }
ReRemoveSharedVariablesDriver >> configureRefactoring [

	super configureRefactoring.
	refactoring prepareForInteractiveMode
]

{ #category : 'configuration' }
ReRemoveSharedVariablesDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		title: 'There are references to variables!';
		items: self breakingChoices;
		display: [ :each | each description ];
		displayIcon: [ :each | self iconNamed: each systemIconName ];
		yourself
]

{ #category : 'configuration' }
ReRemoveSharedVariablesDriver >> instantiateRefactoring [

	^ ReUpFrontPreconditionCheckingCompositeRefactoring new
		model: model; 
		refactorings: (variables collect: [:each | 
				ReRemoveSharedVariableRefactoring model: model remove: each from: class]);
		yourself
]

{ #category : 'initialization' }
ReRemoveSharedVariablesDriver >> scopes: refactoringScopes variables: aCollection for: aClass [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	variables := aCollection.
	class := aClass
]
