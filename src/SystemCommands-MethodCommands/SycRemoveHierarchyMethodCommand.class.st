"
I am a command to remove given methods and the ones in the subclasses.

"
Class {
	#name : #SycRemoveHierarchyMethodCommand,
	#superclass : #SycMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #activation }
SycRemoveHierarchyMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: ClyMoreRefactorsMenuGroup 
		order: 100 
		for: ClyMethod asCalypsoItemContext
]

{ #category : #converting }
SycRemoveHierarchyMethodCommand >> asRefactorings [
	| refactoring |
	refactoring :=  RBRemoveHierarchyMethodRefactoring  
		removeMethods: (methods collect: #selector) 
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

{ #category : #execution }
SycRemoveHierarchyMethodCommand >> execute [

	self executeRefactorings
]
