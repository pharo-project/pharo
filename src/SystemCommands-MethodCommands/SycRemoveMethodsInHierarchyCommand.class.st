"
I am a command to remove given methods and the ones in the subclasses.

"
Class {
	#name : 'SycRemoveMethodsInHierarchyCommand',
	#superclass : 'SycRefactoringMethodCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'converting' }
SycRemoveMethodsInHierarchyCommand >> asRefactorings [

	| refactoring |
	refactoring := ReRemoveMethodsInHierarchyRefactoring
				      		selectors: (methods collect: [ :each | each selector ])
							from: methods first origin.
	^ OrderedCollection with: refactoring
]

{ #category : 'accessing' }
SycRemoveMethodsInHierarchyCommand >> defaultMenuIconName [
	^ #remove
]

{ #category : 'accessing' }
SycRemoveMethodsInHierarchyCommand >> defaultMenuItemName [
	^'Remove and the ones in subclasses'
]

{ #category : 'initialization' }
SycRemoveMethodsInHierarchyCommand >> setUpModelFromContext: aToolContext [
]
