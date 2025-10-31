"
I am a change manager keeping track of all changes applied by the refactory manager. Typically there is only a single
instance of me used in the system, so all changes can be globally added/inspected/ executed and undone/redone.

The size of my undo list is configurable in the settings browser.
"
Class {
	#name : 'ReChangeManager',
	#superclass : 'Object',
	#instVars : [
		'undo',
		'redo',
		'undoPointers',
		'isPerformingRefactoring',
		'changeFactory'
	],
	#classVars : [
		'Counter',
		'Instance',
		'UndoSize'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'change - factory' }
ReChangeManager class >> changeFactory [

	^ self instance changeFactory
]

{ #category : 'class initialization' }
ReChangeManager class >> counter [

	^ Counter ifNil: [ Counter := 0 ]
]

{ #category : 'class initialization' }
ReChangeManager class >> initialize [

	self nuke.
	UndoSize := 20
]

{ #category : 'instance creation' }
ReChangeManager class >> instance [

	^ Instance ifNil: [ Instance := self basicNew initialize ]
]

{ #category : 'world menu' }
ReChangeManager class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'Undo last refactoring')
				action: [self instance undoLastRefactoring];
				parent: #Refactoring;
				help: 'Undo last refactoring';
				order: 10;
				iconName: #smallUndo
]

{ #category : 'instance creation' }
ReChangeManager class >> new [

	^ self shouldNotImplement
]

{ #category : 'actions' }
ReChangeManager class >> nextCounter [

	Counter := self counter + 1.
	^ self counter
]

{ #category : 'public' }
ReChangeManager class >> nuke [

	Instance ifNil: [ ^ self ].

	Instance release.
	Instance := nil
]

{ #category : 'actions' }
ReChangeManager class >> resetCounter [

	Counter := nil
]

{ #category : 'class initialization' }
ReChangeManager class >> undoSize [

	^ UndoSize
]

{ #category : 'class initialization' }
ReChangeManager class >> undoSize: anInteger [

	UndoSize := anInteger max: 0
]

{ #category : 'public' }
ReChangeManager class >> unload [

	self nuke
]

{ #category : 'public access' }
ReChangeManager >> addUndo: aRefactoringChange [
	
	undo push: aRefactoringChange.
	undo size > UndoSize
		ifTrue: [ 
			"Remove the element at the bottom of the stack"
			undo removeLast 
		].
	redo := OrderedCollection new
]

{ #category : 'public access' }
ReChangeManager >> addUndoPointer: aNumber [
	undoPointers push: aNumber
]

{ #category : 'change-factory' }
ReChangeManager >> changeFactory [

	^ changeFactory
]

{ #category : 'private' }
ReChangeManager >> clearUndoRedoList [
	undo := Stack new.
	redo := OrderedCollection new.
	undoPointers := Stack new.
	self class resetCounter
]

{ #category : 'initialization' }
ReChangeManager >> connectToChanges [

	self class codeChangeAnnouncer weak
		when: PackageTagAnnouncement,
				ClassAdded,
				ClassModifiedClassDefinition,
				ClassRemoved,
				ClassRenamed,
				MethodAdded,
				MethodModified,
				MethodRemoved, 
				ProtocolAnnouncement
		send: #update:
		to: self
]

{ #category : 'initialization' }
ReChangeManager >> disconnectFromChanges [

	self class codeChangeAnnouncer unsubscribe: self
]

{ #category : 'testing' }
ReChangeManager >> hasRedoableOperations [
	^ redo isNotEmpty
]

{ #category : 'testing' }
ReChangeManager >> hasUndoableOperations [
	^ undo isNotEmpty
]

{ #category : 'public access' }
ReChangeManager >> ignoreChangesWhile: aBlock [
	isPerformingRefactoring
		ifTrue: [ ^ aBlock value ].
	isPerformingRefactoring := true.
	aBlock ensure: [ isPerformingRefactoring := false ]
]

{ #category : 'initialization' }
ReChangeManager >> initialize [

	super initialize.
	changeFactory := ReChangeFactory new.
	self clearUndoRedoList.
	isPerformingRefactoring := false.
	self connectToChanges
]

{ #category : 'public access' }
ReChangeManager >> lastUndoPointer [
	^ undoPointers pop
]

{ #category : 'public access' }
ReChangeManager >> performChanges: aRefactoringChangesList [

	| compositeChange |
	aRefactoringChangesList ifEmpty: [ ^ self ].
	compositeChange := ReCompositeChange new.
	compositeChange onSystemEnvironment:
		aRefactoringChangesList first onSystemEnvironment.
	compositeChange changes: aRefactoringChangesList.
	self performCompositeChange: compositeChange
]

{ #category : 'public access' }
ReChangeManager >> performCompositeChange: aCompositeChange [

	self ignoreChangesWhile: [
		self addUndo: aCompositeChange execute.
		self addUndoPointer: self class nextCounter ]
]

{ #category : 'public access' }
ReChangeManager >> redoChange [
	^ redo last
]

{ #category : 'public access' }
ReChangeManager >> redoOperation [
	redo ifEmpty: [ ^ self ].
	self ignoreChangesWhile: [
		| change |
		change := redo removeLast.
		undo add: change execute ]
]

{ #category : 'initialization' }
ReChangeManager >> release [

	self disconnectFromChanges
]

{ #category : 'public access' }
ReChangeManager >> undoChange [

	^ undo pop
]

{ #category : 'menu' }
ReChangeManager >> undoLastRefactoring [

	self undoPointers ifNotEmpty: [
	[ |limit list|
		list := OrderedCollection new.
		limit := self lastUndoPointer .
		1 to: limit do:[ :i | list add: self undoChange ].
		list do: [ :e | e execute ].
		^ list
	 ] asJob
		title: 'Refactoring';
		run]
	ifEmpty: [ self inform: 'There aren''t refactorings to undo.'  ]
]

{ #category : 'public access' }
ReChangeManager >> undoOperation [

	undo ifEmpty: [ ^ self ].
	self ignoreChangesWhile: [
		| change |
		"Retrieve the element at the top"
		change := self undoChange.
		redo add: change execute ]
]

{ #category : 'public access' }
ReChangeManager >> undoPointers [

	^ undoPointers
]

{ #category : 'updating' }
ReChangeManager >> update: anEvent [

	isPerformingRefactoring ifFalse: [ self clearUndoRedoList ]
]
