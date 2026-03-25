"
I'm an interaction object responsible for driving the analysis and interactive removal of classes.
In particular I propose different refactorings based on the situation:
- push state of the remove class to children
- reparent the children to their grand-parent.

"
Class {
	#name : 'ReViolentRemoveClassDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'classes',
		'haveNoReferences'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReViolentRemoveClassDriver class >> driverName [

	^ 'Remove class(es)'
]

{ #category : 'accessing' }
ReViolentRemoveClassDriver class >> shouldPreviewChangesByDefault [

	^ false
]

{ #category : 'execution' }
ReViolentRemoveClassDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
	haveNoReferences isFalse ifTrue: [
		items add: (ReBrowseClassReferencesChoice new driver: self) ].
	^ items 
	
]

{ #category : 'actions' }
ReViolentRemoveClassDriver >> browseReferences [

	(application toolNamed: #messageList) browse: (haveNoReferences violators collect: [ :ref | ref compiledMethod ])
]

{ #category : 'actions' }
ReViolentRemoveClassDriver >> changes [
	
	^  refactoring removeClassChanges

]

{ #category : 'configuration' }
ReViolentRemoveClassDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		  extent: 400 @ 1200;
		  title: 'There are potential breaking changes!';
		  message: self labelBasedOnBreakingChanges;
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ]; 
yourself
]

{ #category : 'configuration' }
ReViolentRemoveClassDriver >> instantiateRefactoring [

	^ RBRemoveClassesTransformation model: model classNames: (classes collect: [ :cl | cl name ])
]

{ #category : 'ui - dialogs' }
ReViolentRemoveClassDriver >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream |
			  haveNoReferences violationMessageOn: stream.
			  stream cr.
			  stream nextPutAll: 'Select a strategy' ]
]

{ #category : 'execution' }
ReViolentRemoveClassDriver >> runRefactoring [

	self configureRefactoring.
	refactoring failedApplicabilityPreconditions ifNotEmpty: [ ^ self inform: 'The class should exist and not be a metaclass' ].
	self setBreakingChangesPreconditions.
	haveNoReferences check
		ifTrue: [ self applyChanges ]
		ifFalse: [ self handleBreakingChanges ]
]

{ #category : 'initialization' }
ReViolentRemoveClassDriver >> scopes: refactoringScopes classes: aColclasses [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	classes := aColclasses
]

{ #category : 'private execution' }
ReViolentRemoveClassDriver >> setBreakingChangesPreconditions [
	
	haveNoReferences := refactoring preconditionHaveNoReferences.

	
]
