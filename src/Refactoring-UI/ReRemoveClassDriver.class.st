"
I'm an interaction object responsible for driving the analysis and interactive removal of classes.
In particular I propose different refactorings based on the situation:
- push state of the remove class to children
- reparent the children to their grand-parent.

"
Class {
	#name : 'ReRemoveClassDriver',
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

{ #category : 'execution' }
ReRemoveClassDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
	items add: (ReRemoveClassReparentChoice new
						driver: self;
						classesHaveSubclasses: noSubclasses isFalse;
						emptyClasses: emptyClasses isTrue).
	(noSubclasses isFalse and: [ emptyClasses isFalse ]) ifTrue: [
		items add: (ReRemoveClassAndPushStateToSubclassChoice new driver: self) ].
	haveNoReferences isFalse ifTrue: [
		items add: (ReBrowseClassReferencesChoice new driver: self) ].
	^ items 
	
]

{ #category : 'actions' }
ReRemoveClassDriver >> browseReferences [

	haveNoReferences violators keysAndValuesDo: [ :violator :references | "we should pass the application! "
		StMessageBrowser
			browse: (references collect: [ :ref | ref compiledMethod ])
			asSendersOf: violator realClass ]
]

{ #category : 'actions' }
ReRemoveClassDriver >> changes [
	
	^  refactoring removeClassChanges

]

{ #category : 'initialization' }
ReRemoveClassDriver >> configureRefactoring [

	refactoring := ReRemoveClassRefactoring
		               model: model
		               classNames: (classes collect: [ :cl | cl name ]).
	refactoring prepareForInteractiveMode
]

{ #category : 'configuration' }
ReRemoveClassDriver >> defaultSelectDialog [

	^ SpSelectDialog new
		    title: 'There are potential breaking changes!';
		          label: self labelBasedOnBreakingChanges;
		          items: self breakingChoices;
		          display: [ :each | each description ];
		          displayIcon: [ :each | self iconNamed: each systemIconName ];
		          openModal
	
]

{ #category : 'execution' }
ReRemoveClassDriver >> handleBreakingChanges [

	| select |
	select := self selectDialog.
	select ifNotNil: [ select action ]
]

{ #category : 'ui - dialogs' }
ReRemoveClassDriver >> labelBasedOnBreakingChanges [

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
ReRemoveClassDriver >> removeClassAndPushStateToSubclasses [

	refactoring := ReRemoveClassPushingStateToSubclassesRefactoring
						   model: model
		               classNames: (classes collect: [ :cl | cl name ]).
	self openPreviewWithChanges: refactoring removeClassesChanges
]

{ #category : 'execution' }
ReRemoveClassDriver >> runRefactoring [
	
	self configureRefactoring.
	refactoring failedApplicabilityPreconditions 
		ifNotEmpty: [ ^ self inform: 'The class should exist and not be a metaclass' ].
	self setBreakingChangesPreconditions.
	haveNoReferences check & emptyClasses check & noSubclasses check
			ifTrue: [ self applyChanges ]
			ifFalse: [ self handleBreakingChanges ]

]

{ #category : 'initialization' }
ReRemoveClassDriver >> scopes: refactoringScopes classes: aColclasses [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	classes := aColclasses
]

{ #category : 'private execution' }
ReRemoveClassDriver >> setBreakingChangesPreconditions [
	
	haveNoReferences := refactoring preconditionHaveNoReferences.
	emptyClasses := refactoring preconditionEmptyClasses.
	noSubclasses := refactoring preconditionHaveNoSubclasses

	
]
