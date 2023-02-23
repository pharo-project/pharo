"
I am a command to make a class abstract by adding a method that returns whether the class is abstract
"
Class {
	#name : #SycCMakeAbstractCommand,
	#superclass : #SycClassCmCommand,
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #executing }
SycCMakeAbstractCommand >> executeRefactoring [

	(self refactoringClass class: targetClass) execute
]

{ #category : #accessing }
SycCMakeAbstractCommand >> icon [

	^ self iconNamed: #smallLoadProject
]

{ #category : #accessing }
SycCMakeAbstractCommand >> isApplicable [

	^ context lastSelectedClass isAbstract not
]

{ #category : #testing }
SycCMakeAbstractCommand >> name [
	^ self refactoringClass menuItemString
]

{ #category : #testing }
SycCMakeAbstractCommand >> order [
	^ 40
]

{ #category : #'factory method' }
SycCMakeAbstractCommand >> refactoringClass [

	^ RBMakeClassAbstractTransformation
]
