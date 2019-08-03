"
I am a command to insert new subclass into my target class hierarchy.
All current subclasses of targetClass will become subclasses of new created class
"
Class {
	#name : #SycInsertSubclassCommand,
	#superclass : #SycNewClassCreationCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #activation }
SycInsertSubclassCommand class >> browserContextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation  
		byItemOf: ClyMoreRefactorsMenuGroup  
		order: 20
		for: ClyClass asCalypsoItemContext 
]

{ #category : #accessing }
SycInsertSubclassCommand >> defaultMenuIconName [
	^#smallLoadProject
]

{ #category : #accessing }
SycInsertSubclassCommand >> defaultMenuItemName [
	^'Insert subclass'
]

{ #category : #execution }
SycInsertSubclassCommand >> execute [
	
	| refactoring |
	refactoring := RBAddClassRefactoring
		addClass: newClassName
		superclass: targetClass
		subclasses: targetClass subclasses
		category: targetClass category.
	refactoring execute
]
