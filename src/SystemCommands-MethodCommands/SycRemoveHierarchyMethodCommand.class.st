"
I am a command to remove given methods and the ones in the subclasses.

"
Class {
	#name : #SycRemoveHierarchyMethodCommand,
	#superclass : #SysRefactoringMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #converting }
SycRemoveHierarchyMethodCommand >> asRefactorings [
	| refactoring |
	refactoring :=  RBRemoveHierarchyMethodRefactoring
		removeMethods: (methods collect: [:each | each selector])
		from: methods first origin.
	^ OrderedCollection with: refactoring
]

{ #category : #accessing }
SycRemoveHierarchyMethodCommand >> defaultMenuIconName [
	^ #remove
]

{ #category : #accessing }
SycRemoveHierarchyMethodCommand >> defaultMenuItemName [
	^'Remove and the ones in subclasses'
]

{ #category : #initialization }
SycRemoveHierarchyMethodCommand >> setUpModelFromContext: aToolContext [
]
