"
I am a command to insert new superclass into my target class.
I create new class using supeclass of my target class. And I replace superclass of target class with new created class
"
Class {
	#name : 'SycCmInsertSuperclassCommand',
	#superclass : 'SycCmNewClassCreationCommand',
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'executing' }
SycCmInsertSuperclassCommand >> executeRefactoring [

	(RBInsertNewClassRefactoring className: newClassName)
		superclass: targetClass superclass asString;
		subclasses: { targetClass };
		packageName: targetClass package name;
		tagName: targetClass packageTag name;
		execute
]

{ #category : 'accessing' }
SycCmInsertSuperclassCommand >> icon [
	^ self iconNamed: #smallLoadProject
]

{ #category : 'testing' }
SycCmInsertSuperclassCommand >> isApplicable [

	^ context lastSelectedClass isTrait not
]

{ #category : 'accessing' }
SycCmInsertSuperclassCommand >> name [
	^'Insert superclass'
]

{ #category : 'accessing' }
SycCmInsertSuperclassCommand >> order [
	^ 30
]
