"
I am a command to insert new subclass into my target class hierarchy.
All current subclasses of targetClass will become subclasses of new created class
"
Class {
	#name : 'SycCmInsertSubclassCommand',
	#superclass : 'SycCmNewClassCreationCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'executing' }
SycCmInsertSubclassCommand >> executeRefactoring [

	self flag: #package. "Remove ifNotNil: when undefined tag will be here."
	(RBInsertNewClassRefactoring className: newClassName)
		superclass: targetClass asString;
		subclasses: targetClass subclasses;
		packageName: targetClass package name;
		tagName: (targetClass packageTag ifNotNil: [ :tag | tag name ]);
		execute
]

{ #category : 'accessing' }
SycCmInsertSubclassCommand >> icon [
	^ self iconNamed: #smallLoadProject
]

{ #category : 'testing' }
SycCmInsertSubclassCommand >> isApplicable [

	^ true
]

{ #category : 'accessing' }
SycCmInsertSubclassCommand >> name [
	^'Insert subclass'
]

{ #category : 'accessing' }
SycCmInsertSubclassCommand >> order [
	^ 20
]
