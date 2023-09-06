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
	refactoring := RBCompositeRefactoring new refactorings:
		               (methods collect: [ :method |
			                RBRemoveHierarchyMethodRefactoring
				                removeMethod: method selector
				                from: method origin ]).
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
