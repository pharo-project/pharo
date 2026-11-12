"
I am a command to make a class abstract by adding a method that returns whether the class is abstract
"
Class {
	#name : 'SycMakeAbstractCommand',
	#superclass : 'SycClassRefactoringCommand',
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'testing' }
SycMakeAbstractCommand >> canBeExecuted [

	^ context lastSelectedClass realClass isAbstract not
]

{ #category : 'executing' }
SycMakeAbstractCommand >> executeRefactoring [

	(self refactoringClass class: targetClass) execute
]

{ #category : 'accessing' }
SycMakeAbstractCommand >> iconName [

	^ #smallLoadProject
]

{ #category : 'testing' }
SycMakeAbstractCommand >> name [
	^ self refactoringClass menuItemString
]

{ #category : 'testing' }
SycMakeAbstractCommand >> order [
	^ 40
]

{ #category : 'factory method' }
SycMakeAbstractCommand >> refactoringClass [

	^ RBMakeClassAbstractTransformation
]
