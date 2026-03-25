"
I'm a model for user interaction for the remove method refactoring.
"
Class {
	#name : 'ReRemoveMethodDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'methods',
		'haveNoSenders'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReRemoveMethodDriver class >> driverName [

	^ 'Remove method(s)'
]

{ #category : 'accessing' }
ReRemoveMethodDriver class >> shouldPreviewChangesByDefault [

	^ false
]

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

	(Smalltalk tools toolNamed: #messageList) browseImplementorsOfAll:
		(haveNoSenders violators collect: [ :each | each value selector ])
]

{ #category : 'execution' }
ReRemoveMethodDriver >> changes [

	^ refactoring removeMethodChanges
]

{ #category : 'configuration' }
ReRemoveMethodDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		title: 'There are ' , haveNoSenders violators size asString , ' methods calling method(s) you want to delete';
		message: 'Select a strategy';
		items: self breakingChoices;
		display: [ :each | each description ];
		displayIcon: [ :each | self iconNamed: each systemIconName ]; 
		yourself
]

{ #category : 'configuration' }
ReRemoveMethodDriver >> instantiateRefactoring [

	| classSelectorMapping |
	classSelectorMapping := methods collect: [ :m | m origin -> m selector ].

	^ ReRemoveMethodsRefactoring
       model: model
       classSelectorMapping: classSelectorMapping
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
