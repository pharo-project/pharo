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

	(RBInsertNewClassRefactoring className: newClassName)
		superclass: targetClass asString;
		packageName: targetClass package name;
		tagName: targetClass packageTag name;
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
