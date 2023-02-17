"
I am a command to create new subclass og given class (my targetClass)
"
Class {
	#name : #SycCmAddSubclassCommand,
	#superclass : #SycCmNewClassCreationCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #executing }
SycCmAddSubclassCommand >> executeRefactoring [

	| refactoring |
	refactoring := RBInsertClassTransformation
		addClass: newClassName
		superclass: targetClass asString
		subclasses:  #()
		category: targetClass category.
	refactoring asRefactoring execute.
]

{ #category : #accessing }
SycCmAddSubclassCommand >> icon [
	^self iconNamed:#add
]

{ #category : #testing }
SycCmAddSubclassCommand >> isApplicable [

	^ true
]

{ #category : #accessing }
SycCmAddSubclassCommand >> name [
	^'New subclass'
]

{ #category : #accessing }
SycCmAddSubclassCommand >> order [
	^ 10100.1
]
