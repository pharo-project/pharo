Class {
	#name : 'ReChangeManagerTest',
	#superclass : 'TestCase',
	#instVars : [
		'manager'
	],
	#category : 'Refactoring-Changes-Tests',
	#package : 'Refactoring-Changes-Tests'
}

{ #category : 'private' }
ReChangeManagerTest >> newMockChange [
	^ ReChangeMock new
]

{ #category : 'running' }
ReChangeManagerTest >> setUp [
	super setUp.
	ReChangeManager nuke.
	manager := ReChangeManager instance
]

{ #category : 'tests' }
ReChangeManagerTest >> testAddUndoAddsTheChangeToTheUndoStackAndEmptiesTheRedoCollection [

	| change |
	change := self newMockChange.
	manager addUndo: change.

	self assert: manager undoChange equals: change
]

{ #category : 'tests' }
ReChangeManagerTest >> testAddUndoWhenTheUndoStackIsFullCorrectlyPutsTheChangeAtTheStackTop [

	| change undoSize |
	undoSize := ReChangeManager undoSize.
	"Fill the undo stack to the top"
	1 to: undoSize do: [ :i | manager addUndo: self newMockChange ].
	"Put an extra element to the undo stack"
	change := self newMockChange.
	manager addUndo: change.

	self assert: manager undoChange equals: change
]

{ #category : 'tests' }
ReChangeManagerTest >> testChangeFactoryIsCorrectlyInitialized [

	self assert: ReChangeManager changeFactory isNotNil
]

{ #category : 'tests' }
ReChangeManagerTest >> testRedoOperationExecutesTheCorrectChange [

	| change1 change2 |
	change1 := self newMockChange.
	change2 := self newMockChange.
	
	manager addUndo: change1.
	manager addUndo: change2.
	manager undoOperation.
	manager undoOperation.
	
	self assert: manager redoChange equals: change1.
	manager redoOperation.
	self assert: manager redoChange equals: change2.
	manager redoOperation.
	self deny: manager hasRedoableOperations. 
]

{ #category : 'tests' }
ReChangeManagerTest >> testRedoingAndUndoingAnOperationExecutesTheCorrectChange [

	| change1 change2 |
	change1 := self newMockChange.
	change2 := self newMockChange.
	
	manager addUndo: change1.
	manager addUndo: change2.
	manager undoOperation.
	manager undoOperation.
	
	self assert: manager redoChange equals: change1.
	manager redoOperation.
	self assert: manager redoChange equals: change2.
	manager undoOperation.
	self assert: manager redoChange equals: change1
]

{ #category : 'tests' }
ReChangeManagerTest >> testUndoOperationAddsTheChangeToTheRedoCollection [

	| change |
	change := self newMockChange.
	manager addUndo: change.

	self deny: manager hasRedoableOperations.

	manager undoOperation.

	self assert: manager hasRedoableOperations.
	self assert: manager redoChange equals: change
]

{ #category : 'tests' }
ReChangeManagerTest >> testUndoOperationAddsTheChangeToTheRedoCollectionInTheCorrectOrder [

	| change1 change2 |
	change1 := self newMockChange.
	change2 := self newMockChange.
	manager addUndo: change1.
	manager addUndo: change2.
	
	self deny: manager hasRedoableOperations.
	
	manager undoOperation.
	self assert: manager redoChange equals: change2.
	
	manager undoOperation.
	self assert: manager redoChange equals: change1
]

{ #category : 'tests' }
ReChangeManagerTest >> testUndoOperationWhenTheUndoStackIsEmptyPreservesTheUndoStackAndTheRedoCollectionEmpty [

	"Ensure that the undo stack and the redo collection are empty"
	self deny: manager hasUndoableOperations.
	self deny: manager hasRedoableOperations.
	manager undoOperation.
	"Both should remain empty"
	self deny: manager hasUndoableOperations.
	self deny: manager hasRedoableOperations
]
