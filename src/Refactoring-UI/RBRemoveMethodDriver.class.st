"
I'm a model for user interaction for the remove method refactoring.
"
Class {
	#name : 'RBRemoveMethodDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'methods',
		'haveNoSenders'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
RBRemoveMethodDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
	items add: (RBRemoveChoice new driver: self).
	items add: (RBRemoveAndShowSendersChoice new driver: self).
	items add: (RBDontRemoveButShowSendersChoice new driver: self).
	^ items
]

{ #category : 'execution' }
RBRemoveMethodDriver >> browseSenders [

	StMessageBrowserPresenter  
		browse: (haveNoSenders violators collect: [ :each | each value methodClass realClass >> each value selector ]) 
		asSendersOf: (refactoring allSelectors )
	"this does not work for multiple selectors remove."
]

{ #category : 'execution' }
RBRemoveMethodDriver >> changes [

	^ refactoring removeMethodChanges
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

	| select |
	select := self selectDialog.
	select ifNotNil: [ select action ]
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

{ #category : 'execution' }
RBRemoveMethodDriver >> selectDialog [

	^ SpSelectDialog new
		  title: 'There are ' , haveNoSenders violators size asString
			  , ' methods calling method(s) you want to delete';
		  label: 'Select a strategy';
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  openModal
]
