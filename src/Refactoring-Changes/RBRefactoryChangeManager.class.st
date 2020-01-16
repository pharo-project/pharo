"
I am a change manager keeping track of all changes applied by the refactory manager. Typically there is only a single
instance of me used in the system, so all changes can be globally added/inspected/ executed and undone/redone.

The size of my undo list is configurable in the settings browser.
"
Class {
	#name : #RBRefactoryChangeManager,
	#superclass : #Object,
	#instVars : [
		'undo',
		'redo',
		'undoPointers',
		'isPerformingRefactoring',
		'changeFactory'
	],
	#classVars : [
		'Instance',
		'UndoSize'
	],
	#category : #'Refactoring-Changes'
}

{ #category : #'change-factory' }
RBRefactoryChangeManager class >> changeFactory [
	^ self instance changeFactory
]

{ #category : #'class initialization' }
RBRefactoryChangeManager class >> initialize [
	self nuke.
	UndoSize := 20
]

{ #category : #'instance creation' }
RBRefactoryChangeManager class >> instance [
	^ Instance ifNil: [ Instance := self basicNew initialize ]
]

{ #category : #'instance creation' }
RBRefactoryChangeManager class >> new [
	^ self shouldNotImplement
]

{ #category : #public }
RBRefactoryChangeManager class >> nuke [
	Instance notNil ifTrue: [ Instance release ].
	Instance := nil
]

{ #category : #settings }
RBRefactoryChangeManager class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #refactoring)
		label: 'Refactoring Engine';
		parent: #codeBrowsing;
		description: 'Settings related to the refactoring tools';
		with: [ 
					(aBuilder setting: #undoSize)
						target: self;
						default: 20;
						label: 'Undo size';
						parentName: #refactoring ]
]

{ #category : #'class initialization' }
RBRefactoryChangeManager class >> undoSize [
	^ UndoSize
]

{ #category : #'class initialization' }
RBRefactoryChangeManager class >> undoSize: anInteger [ 
	UndoSize := anInteger max: 0
]

{ #category : #public }
RBRefactoryChangeManager class >> unload [
	self nuke
]

{ #category : #'public access' }
RBRefactoryChangeManager >> addUndo: aRefactoringChange [
	undo push: aRefactoringChange.
	undo size > UndoSize
		ifTrue: [ undo removeFirst ].
	redo := OrderedCollection new
]

{ #category : #'public access' }
RBRefactoryChangeManager >> addUndoPointer: aNumber [
	undoPointers push: aNumber
]

{ #category : #'change-factory' }
RBRefactoryChangeManager >> changeFactory [
	^ changeFactory
]

{ #category : #private }
RBRefactoryChangeManager >> clearUndoRedoList [
	undo := Stack new.
	redo := OrderedCollection new.
	undoPointers := Stack new
]

{ #category : #initialization }
RBRefactoryChangeManager >> connectToChanges [
	SystemAnnouncer uniqueInstance weak
		when: CategoryAdded,
			CategoryRemoved,
			CategoryRenamed,
			ClassAdded,
			ClassModifiedClassDefinition,
			ClassRemoved,
			ClassRenamed,
			ClassReorganized,
			MethodAdded,
			MethodModified,
			MethodRemoved,
			ProtocolAdded,
			ProtocolRemoved
		send: #update:
		to: self.
]

{ #category : #initialization }
RBRefactoryChangeManager >> disconnectFromChanges [
	SystemAnnouncer uniqueInstance unsubscribe: self
]

{ #category : #testing }
RBRefactoryChangeManager >> hasRedoableOperations [
	^ redo isNotEmpty
]

{ #category : #testing }
RBRefactoryChangeManager >> hasUndoableOperations [
	^ undo isNotEmpty
]

{ #category : #'public access' }
RBRefactoryChangeManager >> ignoreChangesWhile: aBlock [
	isPerformingRefactoring
		ifTrue: [ ^ aBlock value ].
	isPerformingRefactoring := true.
	aBlock ensure: [ isPerformingRefactoring := false ]
]

{ #category : #initialization }
RBRefactoryChangeManager >> initialize [
	super initialize.
	changeFactory := RBRefactoryChangeFactory new.
	self clearUndoRedoList.
	isPerformingRefactoring := false.
	self connectToChanges
]

{ #category : #'public access' }
RBRefactoryChangeManager >> lastUndoPointer [
	^ undoPointers pop
]

{ #category : #'public access' }
RBRefactoryChangeManager >> performChange: aRefactoringChange [
	self ignoreChangesWhile: [ self addUndo: aRefactoringChange execute ]
]

{ #category : #'public access' }
RBRefactoryChangeManager >> redoChange [
	^ redo last
]

{ #category : #'public access' }
RBRefactoryChangeManager >> redoOperation [
	redo ifEmpty: [ ^ self ].
	self ignoreChangesWhile: [ 
		| change |
		change := redo removeLast.
		undo add: change execute ]
]

{ #category : #initialization }
RBRefactoryChangeManager >> release [
	self disconnectFromChanges.
	super release.	
]

{ #category : #'public access' }
RBRefactoryChangeManager >> undoChange [
	^ undo pop
]

{ #category : #'public access' }
RBRefactoryChangeManager >> undoOperation [
	undo ifEmpty: [ ^ self ].
	self ignoreChangesWhile: [ 
		| change |
		change := undo removeLast.
		redo add: change execute ]
]

{ #category : #'public access' }
RBRefactoryChangeManager >> undoPointers [
	^ undoPointers
]

{ #category : #updating }
RBRefactoryChangeManager >> update: anEvent [
	isPerformingRefactoring
		ifFalse: [ self clearUndoRedoList ]
]
