"
I'm a model for user interaction for the remove instance variable refactoring.
"
Class {
	#name : 'RBRemoveInstanceVariablesDriver',
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
RBRemoveInstanceVariablesDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
	items add: (RBBrowseReferencedInstanceVariableChoice new driver: self; yourself).
	items add: (RBRemoveInstanceVariableChoice new driver: self; yourself).
	items add: (RBRemoveInstanceVariableAndShowChoice new driver: self;  yourself).
	^ items
]

{ #category : 'execution' }
RBRemoveInstanceVariablesDriver >> browseInstanceVariableReferences [

	StMessageBrowserPresenter
		browse: refactoring violators
		
]

{ #category : 'execution' }
RBRemoveInstanceVariablesDriver >> changes [

	^ refactoring privateTransform
]

{ #category : 'resources' }
RBRemoveInstanceVariablesDriver >> configureRefactoring [

	refactoring := RBUpFrontPreconditionCheckingCompositeRefactoring new
							model: model; 
							refactorings: (variables collect: [:each | 
									RBRemoveInstanceVariableRefactoring model: model remove: each from: class]);
								yourself.
	refactoring prepareForInteractiveMode
]

{ #category : 'execution' }
RBRemoveInstanceVariablesDriver >> defaultSelectDialog [

	^ SpSelectDialog new
		  title: 'There are references to variables!';
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  openModal
]

{ #category : 'execution' }
RBRemoveInstanceVariablesDriver >> handleBreakingChanges [

	| select |
	select := self selectDialog.
	select ifNotNil: [ select action ]
]

{ #category : 'execution' }
RBRemoveInstanceVariablesDriver >> runRefactoring [

	self configureRefactoring.
	
	refactoring checkApplicabilityPreconditions.
	
	refactoring breakingChangePreconditions check
		ifTrue: [ self applyChanges ]
		ifFalse: [ self handleBreakingChanges ]

]

{ #category : 'initialization' }
RBRemoveInstanceVariablesDriver >> scopes: refactoringScopes variables: aCollection for: aClass [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	variables := aCollection.
	class := aClass
]

{ #category : 'execution' }
RBRemoveInstanceVariablesDriver >> selectDialog [
	
	^ selectDialog ifNil: [ selectDialog := self defaultSelectDialog ].
	
]
