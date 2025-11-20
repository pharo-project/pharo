"
I am a command to create new subclass og given class (my targetClass)
"
Class {
	#name : 'SycCmInsertSubclassCommand2',
	#superclass : 'SycCmNewClassCreationCommand',
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'executing' }
SycCmInsertSubclassCommand2 >> executeRefactoring [
	"The class name is necessary so the browser can select the new subclass"
	| driver |
	driver := (ReInsertSubclassDriver 
		superclass: self targetClass 
		packageNames: self packageNames)
			scopes: refactoringScopes.
	driver runRefactoring
		ifFalse: [ ^ self ].
	newClassName := driver subclass.
]

{ #category : 'accessing' }
SycCmInsertSubclassCommand2 >> icon [
	^self iconNamed:#add
]

{ #category : 'testing' }
SycCmInsertSubclassCommand2 >> isApplicable [

	^ context lastSelectedClass isTrait not
]

{ #category : 'accessing' }
SycCmInsertSubclassCommand2 >> name [
	^ '(R) Insert subclass'
]

{ #category : 'accessing' }
SycCmInsertSubclassCommand2 >> newClassName [

	^ newClassName 
]

{ #category : 'accessing' }
SycCmInsertSubclassCommand2 >> newClassName: aString [ 

	newClassName := aString asSymbol
]

{ #category : 'accessing' }
SycCmInsertSubclassCommand2 >> order [
	^ 10100.1
]

{ #category : 'accessing' }
SycCmInsertSubclassCommand2 >> packageNames [
	"Answer a <Collection> of <Symbol> of the packages available to the new class"
	
	^ PackageOrganizer default packageNames
]

{ #category : 'preparation' }
SycCmInsertSubclassCommand2 >> prepareFullExecution [
	
	refactoringScopes := context refactoringScopes.
	targetClass := context lastSelectedClass.

]
