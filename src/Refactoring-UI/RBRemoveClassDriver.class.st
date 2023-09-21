Class {
	#name : 'RBRemoveClassDriver',
	#superclass : 'RBDriver',
	#instVars : [
		'classes',
		'haveNoReferences',
		'empty',
		'noSubclasses'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'resources' }
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
	items add: (RBRemoveClassReparentChoice new driver: self).
	empty violators ifNotEmpty: [
		items add:
			(RBRemoveClassAndPushStateToSubclassChoice new driver: self) ].
	haveNoReferences violators ifNotEmpty: [
		items add: (RBBrowseClassReferencesChoice new driver: self) ].
	
	select := SpSelectDialog new
		          title: 'There are potentital breaking changes!';
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

	| stream |
	stream := String new writeStream.
	
	empty printOn: stream.
	noSubclasses printOn: stream.
	haveNoReferences printOn: stream.
	
	stream nextPutAll: 'Select a strategy'.
	
	^ stream contents
]

{ #category : 'removing' }
RBRemoveClassDriver >> removeClassChanges [

	self openPreviewWithChanges: refactoring removeClassChanges

]

{ #category : 'execution' }
RBRemoveClassDriver >> runRefactoring [
	
	self configureRefactoring.
	refactoring applicabilityPreconditions check ifFalse: [
		RBRefactoringError signal: refactoring applicabilityPreconditions errorString ].
	
	haveNoReferences := refactoring preconditionHaveNoReferences.
	empty := refactoring preconditionEmptyClasses.
	noSubclasses := refactoring preconditionHaveNoSubclasses.

	haveNoReferences check & empty check & noSubclasses check
			ifTrue: [ self openPreviewWithChanges: refactoring generateChanges ]
			ifFalse: [ self handleBreakingChanges ]

]

{ #category : 'initialization' }
RBRemoveClassDriver >> scopes: refactoringScopes classes: aColclasses [
	
	scopes := refactoringScopes.
	classes := aColclasses
]
