"
I am a command to create new subclass og given class (my targetClass)
"
Class {
	#name : 'SycCmAddSubclassCommand',
	#superclass : 'SycCmNewClassCreationCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'executing' }
SycCmAddSubclassCommand >> executeRefactoring [

	self flag: #package. "Remove ifNotNil: when undefined tag will be here."
	(RBInsertNewClassRefactoring className: newClassName)
		superclass: targetClass asString;
		packageName: targetClass package name;
		tagName: (targetClass packageTag ifNotNil: [ :tag | tag name ]);
		execute
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> icon [
	^self iconNamed:#add
]

{ #category : 'testing' }
SycCmAddSubclassCommand >> isApplicable [

	^ true
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> name [
	^'New subclass'
]

{ #category : 'accessing' }
SycCmAddSubclassCommand >> order [
	^ 10100.1
]
