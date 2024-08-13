"
I'm a model for user interaction for the remove method refactoring.
"
Class {
	#name : 'ReRemoveMethodDriver',
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
ReRemoveMethodDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
	items add: (ReRemoveChoice new driver: self).
	items add: (ReRemoveAndShowSendersChoice new driver: self).
	items add: (ReDontRemoveButShowSendersChoice new driver: self).
	^ items
]

{ #category : 'execution' }
ReRemoveMethodDriver >> browseSenders [

	StMessageBrowser  
		browse: (haveNoSenders violators collect: [ :each | each value methodClass realClass >> each value selector ]) 
		asSendersOf: (refactoring allSelectors )
	"this does not work for multiple selectors remove."
]

{ #category : 'execution' }
ReRemoveMethodDriver >> changes [

	^ refactoring removeMethodChanges
]

{ #category : 'resources' }
ReRemoveMethodDriver >> configureRefactoring [

	| classSelectorMapping |
	classSelectorMapping := methods collect: [ :m | m origin -> m selector ].

	refactoring := ReRemoveMethodsRefactoring
			            model: model
			            classSelectorMapping: classSelectorMapping
]

{ #category : 'execution' }
ReRemoveMethodDriver >> handleBreakingChanges [

	| select |
	select := self selectDialog.
	select ifNotNil: [ select action ]
]

{ #category : 'execution' }
ReRemoveMethodDriver >> runRefactoring [

	self configureRefactoring.
	refactoring failedApplicabilityPreconditions 
		ifNotEmpty: [ ^ self inform: 'The method does not exist' ].
	
	haveNoSenders := refactoring preconditionHaveNoSenders.

	haveNoSenders check
			ifTrue: [ self applyChanges ]
			ifFalse: [ self handleBreakingChanges ]
]

{ #category : 'initialization' }
ReRemoveMethodDriver >> scopes: refactoringScopes method: aMethod [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	methods := { aMethod }
]

{ #category : 'initialization' }
ReRemoveMethodDriver >> scopes: refactoringScopes methods: aMethods [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	methods := aMethods
]

{ #category : 'execution' }
ReRemoveMethodDriver >> selectDialog [

	^ SpSelectDialog new
		  title: 'There are ' , haveNoSenders violators size asString
			  , ' methods calling method(s) you want to delete';
		  label: 'Select a strategy';
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  openModal
]
