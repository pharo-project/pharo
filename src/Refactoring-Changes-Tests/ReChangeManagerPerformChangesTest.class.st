Class {
	#name : 'ReChangeManagerPerformChangesTest',
	#superclass : 'TestCase',
	#instVars : [
		'manager'
	],
	#category : 'Refactoring-Changes-Tests',
	#package : 'Refactoring-Changes-Tests'
}

{ #category : 'instance creation' }
ReChangeManagerPerformChangesTest >> listOfMockChangesWithSize: aNumber [

	^ (1 to: aNumber) collect: [ :i | ReChangeMock new name: i asString ]
]

{ #category : 'instance creation' }
ReChangeManagerPerformChangesTest >> newMockChange [

	^ ReChangeMock new
]

{ #category : 'running' }
ReChangeManagerPerformChangesTest >> setUp [

	super setUp.
	ReChangeManager nuke.
	manager := ReChangeManager instance
]

{ #category : 'tests' }
ReChangeManagerPerformChangesTest >> testPerformChanges [

	| changes |
	changes := self listOfMockChangesWithSize: 5.
	manager performChanges: changes.
	
	manager undoLastRefactoring.
	
	self deny: manager hasUndoableOperations
]

{ #category : 'tests' }
ReChangeManagerPerformChangesTest >> testPerformChangesWhenChangesBiggerThanUndoSizeExpectCorrectChangesUndone [

	| changes undoneChanges |
	changes := self listOfMockChangesWithSize: manager class undoSize + 1.
	manager performChanges: changes.
	
	undoneChanges := manager undoLastRefactoring.
	
	self assertCollection: undoneChanges first changes asArray equals: changes reversed asArray
]

{ #category : 'tests' }
ReChangeManagerPerformChangesTest >> testPerformChangesWhenChangesSmallerThanUndoSizeExpectCorrectChangesUndone [

	| changes undoneChanges |
	changes := self listOfMockChangesWithSize: 5.
	manager performChanges: changes.
	
	undoneChanges := manager undoLastRefactoring.
	
	self assertCollection: undoneChanges first changes asArray equals: changes reversed asArray
]
