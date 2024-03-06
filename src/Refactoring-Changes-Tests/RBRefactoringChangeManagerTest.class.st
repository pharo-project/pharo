Class {
	#name : 'RBRefactoringChangeManagerTest',
	#superclass : 'TestCase',
	#instVars : [
		'manager'
	],
	#category : 'Refactoring-Changes-Tests',
	#package : 'Refactoring-Changes-Tests'
}

{ #category : 'private' }
RBRefactoringChangeManagerTest >> newMockChange [
	^ RBRefactoringChangeMock new
]

{ #category : 'tests' }
RBRefactoringChangeManagerTest >> testAddUndoAddsTheChangeToTheUndoStackAndEmptiesTheRedoCollection [

	| change |
	change := self newMockChange.
	manager addUndo: change.

	self assert: manager undoChange equals: change
]

{ #category : 'tests' }
RBRefactoringChangeManagerTest >> testAddUndoWhenTheUndoStackIsFullCorrectlyPutsTheChangeAtTheStackTop [

	| change undoSize |
	undoSize := RBRefactoryChangeManager undoSize.
	"Fill the undo stack to the top"
	1 to: undoSize do: [ :i | manager addUndo: self newMockChange ].
	"Put an extra element to the undo stack"
	change := self newMockChange.
	manager addUndo: change.

	self assert: manager undoChange equals: change
]

{ #category : 'tests' }
RBRefactoringChangeManagerTest >> testChangeFactoryIsCorrectlyInitialized [

	self assert: RBRefactoryChangeManager changeFactory isNotNil
]

{ #category : 'tests' }
RBRefactoringChangeManagerTest >> testRedoOperationExecutesTheCorrectChange [

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
RBRefactoringChangeManagerTest >> testRedoingAndUndoingAnOperationExecutesTheCorrectChange [

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
RBRefactoringChangeManagerTest >> testUndoOperationAddsTheChangeToTheRedoCollection [

	| change |
	change := self newMockChange.
	manager addUndo: change.

	self deny: manager hasRedoableOperations.

	manager undoOperation.

	self assert: manager hasRedoableOperations.
	self assert: manager redoChange equals: change
]

{ #category : 'tests' }
RBRefactoringChangeManagerTest >> testUndoOperationAddsTheChangeToTheRedoCollectionInTheCorrectOrder [

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
RBRefactoringChangeManagerTest >> testUndoOperationWhenTheUndoStackIsEmptyPreservesTheUndoStackAndTheRedoCollectionEmpty [

	"Ensure that the undo stack and the redo collection are empty"
	self deny: manager hasUndoableOperations.
	self deny: manager hasRedoableOperations.
	manager undoOperation.
	"Both should remain empty"
	self deny: manager hasUndoableOperations.
	self deny: manager hasRedoableOperations
]
