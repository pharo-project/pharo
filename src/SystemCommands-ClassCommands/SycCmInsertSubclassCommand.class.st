"
I am a command to insert new subclass into my target class hierarchy.
All current subclasses of targetClass will become subclasses of new created class
"
Class {
	#name : #SycCmInsertSubclassCommand,
	#superclass : #SycCmNewClassCreationCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #executing }
SycCmInsertSubclassCommand >> executeRefactoring [

	| refactoring |
	refactoring := RBInsertClassTransformation
		addClass: newClassName
		superclass: targetClass asString
		subclasses: targetClass subclasses
		category: targetClass category.
	refactoring asRefactoring execute
]

{ #category : #accessing }
SycCmInsertSubclassCommand >> icon [
	^ self iconNamed: #smallLoadProject
]

{ #category : #testing }
SycCmInsertSubclassCommand >> isApplicable [

	^ true
]

{ #category : #accessing }
SycCmInsertSubclassCommand >> name [
	^'Insert subclass'
]

{ #category : #accessing }
SycCmInsertSubclassCommand >> order [
	^ 20
]
