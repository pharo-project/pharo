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

	(RBMakeClassAbstractTransformation class: targetClass) execute
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
	^ '(T) Make Abstract'
]

{ #category : #testing }
SycCMakeAbstractCommand >> order [
	^ 40
]
