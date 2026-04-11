"
I'm a model for user interaction for the remove instance variable refactoring.
"
Class {
	#name : 'ReRemoveInstanceVariablesDriver',
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
ReRemoveInstanceVariablesDriver class >> driverName [

	^ 'Remove instance variable(s)'
]

{ #category : 'accessing' }
ReRemoveInstanceVariablesDriver class >> shouldPreviewChangesByDefault [

	^ false
]

{ #category : 'execution' }
ReRemoveInstanceVariablesDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
	items add: (ReBrowseReferencedInstanceVariableChoice new driver: self; yourself).
	items add: (ReRemoveInstanceVariableChoice new driver: self; yourself).
	items add: (ReRemoveInstanceVariableAndShowChoice new driver: self;  yourself).
	^ items
]

{ #category : 'execution' }
ReRemoveInstanceVariablesDriver >> browseInstanceVariableReferences [

	| violators |
	violators := refactoring refactorings flatCollect: [ :each | each preconditionHasReferences violators ].

	(Smalltalk tools toolNamed: #messageList) browse: violators
]

{ #category : 'configuration' }
ReRemoveInstanceVariablesDriver >> configureRefactoring [

	super configureRefactoring.
	refactoring prepareForInteractiveMode
]

{ #category : 'configuration' }
ReRemoveInstanceVariablesDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		  title: 'There are references to variables!';
		  message: self labelBasedOnBreakingChanges;
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  yourself
]

{ #category : 'configuration' }
ReRemoveInstanceVariablesDriver >> instantiateRefactoring [

	^ ReUpFrontPreconditionCheckingCompositeRefactoring new
		model: model; 
		refactorings: (variables collect: [:each | 
				ReRemoveInstanceVariableRefactoring model: model remove: each from: class]);
		yourself
]

{ #category : 'ui - dialogs' }
ReRemoveInstanceVariablesDriver >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream |
			  refactoring failedBreakingChangePreconditions do: [ :cond |
					  cond check ifFalse: [
							  cond violationMessageOn: stream.
							  stream cr ] ].
			  stream nextPutAll: 'Select the strategies to apply' ]
]

{ #category : 'initialization' }
ReRemoveInstanceVariablesDriver >> scopes: refactoringScopes variables: aCollection for: aClass [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	variables := aCollection.
	class := aClass
]
