Class {
	#name : 'RBRemoveSharedVariablesDriver2',
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
RBRemoveSharedVariablesDriver2 >> applyChanges [

	self openPreviewWithChanges: refactoring privateTransform
]

{ #category : 'execution' }
RBRemoveSharedVariablesDriver2 >> browseSharedVariableReferences [

	StMessageBrowserPresenter
		browse: refactoring violators
		
]

{ #category : 'resources' }
RBRemoveSharedVariablesDriver2 >> configureRefactoring [

	refactoring := RBUpFrontPreconditionCheckingCompositeRefactoring new
							model: model; 
							refactorings: (variables collect: [:each | 
									RBRemoveSharedVariableRefactoring2 model: model remove: each from: class]);
								yourself.
	refactoring prepareForInteractiveMode
]

{ #category : 'execution' }
RBRemoveSharedVariablesDriver2 >> handleBreakingChanges [

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
RBRemoveSharedVariablesDriver2 >> runRefactoring [

	self configureRefactoring.
	
	refactoring checkApplicabilityPreconditions.
	
	refactoring breakingChangePreconditions check
		ifTrue: [ self applyChanges ]
		ifFalse: [ self handleBreakingChanges ]

]

{ #category : 'initialization' }
RBRemoveSharedVariablesDriver2 >> scopes: refactoringScopes variables: aCollection for: aClass [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	variables := aCollection.
	class := aClass
]
