"
I'm an interaction class in charge of stirring the application or not of removing shared variables.
"
Class {
	#name : 'RBRemoveSharedVariablesDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'class',
		'variables'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
RBRemoveSharedVariablesDriver >> browseSharedVariableReferences [

	StMessageBrowserPresenter
		browse: refactoring violators
		
]

{ #category : 'execution' }
RBRemoveSharedVariablesDriver >> changes [

	^ refactoring privateTransform
]

{ #category : 'resources' }
RBRemoveSharedVariablesDriver >> configureRefactoring [

	refactoring := RBUpFrontPreconditionCheckingCompositeRefactoring new
							model: model; 
							refactorings: (variables collect: [:each | 
									RBRemoveSharedVariableRefactoring model: model remove: each from: class]);
								yourself.
	refactoring prepareForInteractiveMode
]

{ #category : 'execution' }
RBRemoveSharedVariablesDriver >> handleBreakingChanges [

	| select items |
	items := OrderedCollection new.
	items add: (RBBrowseReferencedSharedVariableChoice new driver: self ; yourself).
	items add: (RBRemoveSharedVariableChoice new driver: self; yourself).
	items add: (RBRemoveSharedVariableAndShowChoice new driver: self; yourself).
	
	select := SpSelectDialog new
		          title: 'There are references to variables!';
		          items: items;
		          display: [ :each | each description ];
		          displayIcon: [ :each |  self iconNamed: each systemIconName ];
		          openModal.

	select ifNotNil: [ select action ]
]

{ #category : 'execution' }
RBRemoveSharedVariablesDriver >> runRefactoring [

	self configureRefactoring.
	
	refactoring checkApplicabilityPreconditions.
	
	refactoring breakingChangePreconditions check
		ifTrue: [ self applyChanges ]
		ifFalse: [ self handleBreakingChanges ]

]

{ #category : 'initialization' }
RBRemoveSharedVariablesDriver >> scopes: refactoringScopes variables: aCollection for: aClass [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	variables := aCollection.
	class := aClass
]
