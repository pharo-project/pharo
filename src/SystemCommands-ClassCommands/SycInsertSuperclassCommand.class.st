"
I am a command to insert new superclass into my target class.
I create new class using supeclass of my target class. And I replace superclass of target class with new created class
"
Class {
	#name : 'SycInsertSuperclassCommand',
	#superclass : 'SycNewClassCreationCommand',
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'execution' }
SycInsertSuperclassCommand >> executeRefactoring [
	"The class name is necessary so the browser can select the new subclass"

	| driver |
	driver := (ReInsertSuperclassDriver
		           superclass: self targetClass
		           superclassParent: self targetClass superclass
		           packageNames: self packageNames) scopes: refactoringScopes. "Pay attention superclass meansthe class above which the new class was inserted"

	driver runRefactoring.
	newClassName := driver superclass name
]

{ #category : 'accessing' }
SycInsertSuperclassCommand >> iconName [

	^ #smallLoadProject
]

{ #category : 'accessing' }
SycInsertSuperclassCommand >> name [
	^ '(R) Insert superclass'
]

{ #category : 'accessing' }
SycInsertSuperclassCommand >> order [
	^ 30
]
