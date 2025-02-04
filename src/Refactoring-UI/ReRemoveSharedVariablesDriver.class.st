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

	StMessageBrowser browse: refactoring violators
]

{ #category : 'resources' }
ReRemoveSharedVariablesDriver >> configureRefactoring [

	refactoring := ReUpFrontPreconditionCheckingCompositeRefactoring new
							model: model; 
							refactorings: (variables collect: [:each | 
									ReRemoveSharedVariableRefactoring model: model remove: each from: class]);
								yourself.
	refactoring prepareForInteractiveMode
]

{ #category : 'execution' }
ReRemoveSharedVariablesDriver >> defaultSelectDialog [

	^ SpSelectDialog new
		          title: 'There are references to variables!';
		          items: self breakingChoices;
		          display: [ :each | each description ];
		          displayIcon: [ :each |  self iconNamed: each systemIconName ];
		          openModal.

]

{ #category : 'execution' }
ReRemoveSharedVariablesDriver >> handleBreakingChanges [

	| select  |
	select := self defaultSelectDialog.
	select ifNotNil: [ select action ]
]

{ #category : 'execution' }
ReRemoveSharedVariablesDriver >> runRefactoring [

	self configureRefactoring.
	
	refactoring failedApplicabilityPreconditions 
		ifNotEmpty: [ ^ self inform: 'The shared variable does not exist' ].

	refactoring failedBreakingChangePreconditions
		ifEmpty: [ self applyChanges ]
		ifNotEmpty: [ self handleBreakingChanges ]

]

{ #category : 'initialization' }
ReRemoveSharedVariablesDriver >> scopes: refactoringScopes variables: aCollection for: aClass [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	variables := aCollection.
	class := aClass
]
