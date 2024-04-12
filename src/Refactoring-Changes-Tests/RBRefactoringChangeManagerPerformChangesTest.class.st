Class {
	#name : 'RBRefactoringChangeManagerPerformChangesTest',
	#superclass : 'TestCase',
	#instVars : [
		'manager'
	],
	#category : 'Refactoring-Changes-Tests',
	#package : 'Refactoring-Changes-Tests'
}

{ #category : 'instance creation' }
RBRefactoringChangeManagerPerformChangesTest >> listOfMockChangesWithSize: aNumber [

	^ (1 to: aNumber) collect: [ :i | RBRefactoringChangeMock new ]
]

{ #category : 'instance creation' }
RBRefactoringChangeManagerPerformChangesTest >> newMockChange [

	^ RBRefactoringChangeMock new
]

{ #category : 'running' }
RBRefactoringChangeManagerPerformChangesTest >> setUp [

	super setUp.
	RBRefactoryChangeManager nuke.
	manager := RBRefactoryChangeManager instance
]

{ #category : 'tests' }
RBRefactoringChangeManagerPerformChangesTest >> testPerformChanges [

	| changes |
	changes := self listOfMockChangesWithSize: 5.
	manager performChanges: changes.
	
	manager undoLastRefactoring.
	
	self deny: manager hasUndoableOperations
]
