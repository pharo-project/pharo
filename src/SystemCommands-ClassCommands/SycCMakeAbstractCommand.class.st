"
I am a command to make a class abstract by adding a method that returns whether the class is abstract
"
Class {
	#name : 'SycCMakeAbstractCommand',
	#superclass : 'SycClassCmCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'executing' }
SycCMakeAbstractCommand >> executeRefactoring [

	(RBMakeClassAbstractDriver new
		 scopes: refactoringScopes
		 class: targetClass) runRefactoring

]

{ #category : 'accessing' }
SycCMakeAbstractCommand >> icon [

	^ self iconNamed: #smallLoadProject
]

{ #category : 'testing' }
SycCMakeAbstractCommand >> isApplicable [

	^ context lastSelectedClass realClass isAbstract not
]

{ #category : 'accessing' }
SycCMakeAbstractCommand >> name [

	^ 'Make abstract'
]

{ #category : 'accessing' }
SycCMakeAbstractCommand >> order [
	^ 40
]

{ #category : 'factory method' }
SycCMakeAbstractCommand >> refactoringClass [

	^ RBMakeClassAbstractTransformation
]
