"
I am a command to insert new superclass into my target class.
I create new class using supeclass of my target class. And I replace superclass of target class with new created class
"
Class {
	#name : 'SycCmInsertSuperclassCommand',
	#superclass : 'SycCmNewClassCreationCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'executing' }
SycCmInsertSuperclassCommand >> executeRefactoring [

	self flag: #package. "Remove ifNotNil: when undefined tag will be here."
	(RBInsertNewClassRefactoring className: newClassName)
		superclass: targetClass superclass asString;
		subclasses: { targetClass };
		packageName: targetClass package name;
		tagName: (targetClass packageTag ifNotNil: [ :tag | tag name ]);
		execute
]

{ #category : 'accessing' }
SycCmInsertSuperclassCommand >> icon [
	^ self iconNamed: #smallLoadProject
]

{ #category : 'testing' }
SycCmInsertSuperclassCommand >> isApplicable [

	^ true
]

{ #category : 'accessing' }
SycCmInsertSuperclassCommand >> name [
	^'Insert superclass'
]

{ #category : 'accessing' }
SycCmInsertSuperclassCommand >> order [
	^ 30
]
