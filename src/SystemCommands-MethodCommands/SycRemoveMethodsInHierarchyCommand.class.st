"
I am a command to remove given methods and the ones in the subclasses.

"
Class {
	#name : 'SycRemoveMethodsInHierarchyCommand',
	#superclass : 'SycMethodRefactoringCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'converting' }
SycRemoveMethodsInHierarchyCommand >> asRefactorings [

	| refactoring |
	refactoring := ReRemoveMethodsInHierarchyRefactoring selectors: (self methods collect: [ :each | each selector ]) from: self methods first origin.
	^ OrderedCollection with: refactoring
]

{ #category : 'accessing' }
SycRemoveMethodsInHierarchyCommand >> iconName [

	^ #remove
]

{ #category : 'accessing' }
SycRemoveMethodsInHierarchyCommand >> name [

	^ 'Remove and the ones in subclasses'
]

{ #category : 'accessing' }
SycRemoveMethodsInHierarchyCommand >> order [

	^ 100
]

{ #category : 'initialization' }
SycRemoveMethodsInHierarchyCommand >> setUpModelFromContext: aToolContext [
]
