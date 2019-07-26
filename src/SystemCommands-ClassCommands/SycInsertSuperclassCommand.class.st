"
I am a command to insert new superclass into my target class.
I create new class using supeclass of my target class. And I replace superclass of target class with new created class
"
Class {
	#name : #SycInsertSuperclassCommand,
	#superclass : #SycNewClassCreationCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #accessing }
SycInsertSuperclassCommand >> defaultMenuIconName [
	^#smallLoadProject
]

{ #category : #accessing }
SycInsertSuperclassCommand >> defaultMenuItemName [
	^'Insert superclass'
]

{ #category : #execution }
SycInsertSuperclassCommand >> execute [
	
	| refactoring |
	refactoring := RBAddClassRefactoring
		addClass: newClassName
		superclass: targetClass superclass
		subclasses: {targetClass}
		category: targetClass category.
	refactoring execute
]
