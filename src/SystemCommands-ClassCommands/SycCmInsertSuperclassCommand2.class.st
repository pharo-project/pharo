"
I am a command to insert new superclass into my target class.
I create new class using supeclass of my target class. And I replace superclass of target class with new created class
"
Class {
	#name : 'SycCmInsertSuperclassCommand2',
	#superclass : 'SycCmNewClassCreationCommand',
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'executing' }
SycCmInsertSuperclassCommand2 >> executeRefactoring [
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
SycCmInsertSuperclassCommand2 >> icon [
	^ self iconNamed: #smallLoadProject
]

{ #category : 'testing' }
SycCmInsertSuperclassCommand2 >> isApplicable [

	^ context lastSelectedClass isTrait not
]

{ #category : 'accessing' }
SycCmInsertSuperclassCommand2 >> name [
	^ '(R) Insert superclass'
]

{ #category : 'accessing' }
SycCmInsertSuperclassCommand2 >> order [
	^ 30
]

{ #category : 'accessing' }
SycCmInsertSuperclassCommand2 >> packageNames [
	"Answer a <Collection> of <Symbol> of the packages available to the new class"
	
	^ PackageOrganizer default packageNames
]

{ #category : 'preparation' }
SycCmInsertSuperclassCommand2 >> prepareFullExecution [
	
	refactoringScopes := context refactoringScopes.
	targetClass := context lastSelectedClass.

]
