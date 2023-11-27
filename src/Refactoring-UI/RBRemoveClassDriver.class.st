"
I'm an interaction object responsible for driving the analysis and interactive removal of classes.
In particular I propose different refactorings based on the situation:
- push state of the remove class to children
- reparent the children to their grand-parent.

"
Class {
	#name : 'RBRemoveClassDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'classes',
		'haveNoReferences',
		'noSubclasses',
		'emptyClasses'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'actions' }
RBRemoveClassDriver >> applyChanges [

	self openPreviewWithChanges: refactoring removeClassChanges

]

{ #category : 'actions' }
RBRemoveClassDriver >> browseReferences [

	haveNoReferences violators keysAndValuesDo: [ :violator :references |
		StMessageBrowserPresenter
			browse: (references collect: [ :ref | ref method ])
			asSendersOf: violator realClass ]
]

{ #category : 'actions' }
RBRemoveClassDriver >> changes [
	
	^  refactoring removeClassChanges

]

{ #category : 'initialization' }
RBRemoveClassDriver >> configureRefactoring [

	refactoring := RBRemoveClassRefactoring
		               model: model
		               classNames: (classes collect: [ :cl | cl name ]).
	refactoring prepareForInteractiveMode
]

{ #category : 'execution' }
RBRemoveClassDriver >> handleBreakingChanges [

	| select items |
	items := OrderedCollection new.
	items add: (RBRemoveClassReparentChoice new
						driver: self;
						classesHaveSubclasses: noSubclasses isFalse;
						emptyClasses: emptyClasses isTrue).
	(noSubclasses isFalse and: [ emptyClasses isFalse ]) ifTrue: [
		items add:
			(RBRemoveClassAndPushStateToSubclassChoice new driver: self) ].
	haveNoReferences isFalse ifTrue: [
		items add: (RBBrowseClassReferencesChoice new driver: self) ].
	
	items size = 1
		ifTrue: [ ^ items first action ].
		
	select := SpSelectDialog new
		          title: 'There are potential breaking changes!';
		          label: self labelBasedOnBreakingChanges;
		          items: items;
		          display: [ :each | each description ];
		          displayIcon: [ :each |
			          self iconNamed: each systemIconName ];
		          openModal.

	select ifNotNil: [ select action ]
]

{ #category : 'ui - dialogs' }
RBRemoveClassDriver >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream | 
		emptyClasses violationMessageOn: stream.
		stream cr.
		noSubclasses violationMessageOn: stream.
		stream cr.
		haveNoReferences violationMessageOn: stream.
		stream cr.
		stream nextPutAll: 'Select a strategy' ]
]

{ #category : 'actions' }
RBRemoveClassDriver >> removeClassAndPushStateToSubclasses [

	refactoring := RBRemoveClassPushingStateToSubclassesRefactoring
						   model: model
		               classNames: (classes collect: [ :cl | cl name ]).
	self openPreviewWithChanges: refactoring removeClassesChanges
]

{ #category : 'execution' }
RBRemoveClassDriver >> runRefactoring [
	
	self configureRefactoring.
	refactoring checkApplicabilityPreconditions.
	
	haveNoReferences := refactoring preconditionHaveNoReferences.
	emptyClasses := refactoring preconditionEmptyClasses.
	noSubclasses := refactoring preconditionHaveNoSubclasses.

	haveNoReferences check & emptyClasses check & noSubclasses check
			ifTrue: [ self applyChanges ]
			ifFalse: [ self handleBreakingChanges ]

]

{ #category : 'initialization' }
RBRemoveClassDriver >> scopes: refactoringScopes classes: aColclasses [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	classes := aColclasses
]
