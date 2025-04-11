"
I'm a model for user interaction for the rename method refactoring.
"
Class {
	#name : 'ReMoveMethodsToClassSideDriver',
	#superclass : 'ReInteractionDriver',
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'initialization' }
ReMoveMethodsToClassSideDriver class >> scopes: scopesList methods: aCollectionOfMethods [

	^ self new scopes: scopesList methods: aCollectionOfMethods
]

{ #category : 'execution' }
ReMoveMethodsToClassSideDriver >> breakingChoices [
	
	| items |
	items := OrderedCollection new.
	items add: (ReBrowseMethodChoice new driver: self).
	items add: (ReMoveClassSideChoice new driver: self).
	^ items
]

{ #category : 'execution' }
ReMoveMethodsToClassSideDriver >> browseMethod [
	
	| methods |
	methods := refactoring refactorings collect: [ :each | each method ].
	"the API of the StMessageBrowser is not good we should be able to pass mutiple pairs
	of selector/implementors without the need to spawn multipe browser. 
	Bad UX."
	
	methods do: [:each |
		StMessageBrowser browseImplementorsOf: each selector ]
]

{ #category : 'execution' }
ReMoveMethodsToClassSideDriver >> changes [
	"Here we are experimenting about the protocol to composite.
	Indeed ReCompositeRefactoring >> #privateTransform should not call generateChanges
	and for now this is the case."
	
	refactoring privateTransform2.
	^ refactoring changes

]

{ #category : 'configuration' }
ReMoveMethodsToClassSideDriver >> defaultSelectDialog [

	^ self class defaultSelectDialog
		  title: 'Warning you may break your code';
		  label:
			  'One or more methods are already defined on the class side!';
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  openModal
]

{ #category : 'execution' }
ReMoveMethodsToClassSideDriver >> handleBreakingChanges [

	| select |
	select := self selectDialog.
	select ifNotNil: [ select action ]
]

{ #category : 'execution' }
ReMoveMethodsToClassSideDriver >> runRefactoring [
	
	refactoring failedBreakingChangePreconditions
		ifEmpty: [ self applyChanges ]
		ifNotEmpty: [ self handleBreakingChanges ]

]

{ #category : 'execution' }
ReMoveMethodsToClassSideDriver >> scopes: scopesList methods: methods [

	scopes := scopesList.
	model := self refactoringScopeOn: scopesList first.
	refactoring := ReCompositeRefactoring new
		               model: model;
		               refactorings: (methods collect: [ :m |
					                RBMoveMethodToClassSideRefactoring
						                model: model
						                method: m
						                class: m origin ])
]
