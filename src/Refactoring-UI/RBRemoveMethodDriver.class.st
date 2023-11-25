"
I'm a model for user interaction for the remove method refactoring.
"
Class {
	#name : 'RBRemoveMethodDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'methods',
		'senders',
		'haveNoSenders'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
RBRemoveMethodDriver >> applyChanges [

	 self openPreviewWithChanges: refactoring generateChanges
]

{ #category : 'execution' }
RBRemoveMethodDriver >> browseSenders [

	StMessageBrowserPresenter  
		browse: (senders collect: [ :each | each value methodClass realClass >> each value selector ]) 
		asSendersOf: (refactoring refactorings flatCollect: [ :ref | ref selectors ])
	"this does not work for multiple selectors remove."
]

{ #category : 'resources' }
RBRemoveMethodDriver >> configureRefactoring [

	| classSelectorMapping |
	classSelectorMapping := methods collect: [ :m | m origin -> m selector ].

	refactoring := RBRemoveMethodsRefactoring
			            model: model
			            classSelectorMapping: classSelectorMapping
]

{ #category : 'execution' }
RBRemoveMethodDriver >> handleBreakingChanges [

	| select items |
	items := OrderedCollection new.

	items add: (RBRemoveChoice new driver: self).
	items add: (RBRemoveAndShowSendersChoice new driver: self).
	items add: (RBDontRemoveButShowSendersChoice new driver: self).

	select := SpSelectDialog new
		          title: 'There are ' , haveNoSenders violators size asString
			          , ' methods calling method(s) you want to delete';
		          label: 'Select a strategy';
		          items: items;
		          display: [ :each | each description ];
		          displayIcon: [ :each |
			          self iconNamed: each systemIconName ];
		          openModal.

	select ifNotNil: [ select action ]
]

{ #category : 'execution' }
RBRemoveMethodDriver >> removeMethodChanges [ 

	refactoring refactorings do: [ :ref | ref removeMethodChanges ].
	self openPreviewWithChanges: refactoring changes

]

{ #category : 'execution' }
RBRemoveMethodDriver >> runRefactoring [

	self configureRefactoring.
	refactoring checkApplicabilityPreconditions.
	
	haveNoSenders := refactoring preconditionHaveNoSenders.

	haveNoSenders check
			ifTrue: [ self applyChanges ]
			ifFalse: [ self handleBreakingChanges ]

]

{ #category : 'initialization' }
RBRemoveMethodDriver >> scopes: refactoringScopes method: aMethod [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	methods := { aMethod }
]

{ #category : 'initialization' }
RBRemoveMethodDriver >> scopes: refactoringScopes methods: aMethods [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	methods := aMethods
]
