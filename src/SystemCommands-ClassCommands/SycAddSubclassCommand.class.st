"
I am a command to create new subclass og given class (my targetClass)
"
Class {
	#name : #SycAddSubclassCommand,
	#superclass : #SycNewClassCreationCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #activation }
SycAddSubclassCommand class >> browserContextMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation  
		byItemOf: ClyMoreRefactorsMenuGroup  
		order: 10  
		for: ClyClass asCalypsoItemContext 
]

{ #category : #accessing }
SycAddSubclassCommand >> defaultMenuIconName [
	^#add
]

{ #category : #accessing }
SycAddSubclassCommand >> defaultMenuItemName [
	^'New subclass'
]

{ #category : #execution }
SycAddSubclassCommand >> execute [
	
	| refactoring |
	refactoring := RBAddClassRefactoring
		addClass: newClassName
		superclass: targetClass
		subclasses: #()
		category: targetClass category.
	refactoring execute
]
