"
I am a base class for commands which create new class.
They all needs new class name

Internal Representation and Key Implementation Points.

    Instance Variables
	newClassName:		<String>
"
Class {
	#name : 'SycNewClassCreationCommand',
	#superclass : 'SycClassRefactoringCommand',
	#instVars : [
		'newClassName'
	],
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'testing' }
SycNewClassCreationCommand class >> isAbstract [

	^ self == SycNewClassCreationCommand
]

{ #category : 'execution' }
SycNewClassCreationCommand >> applyCommandResult [
	"Check if user cancelled while viewing changes"
	| newClass |
	
	(self class environment hasClassNamed: newClassName)
		ifFalse: [ ^ self ].
		
	newClass := context system classNamed: newClassName.
	context showClass: newClass
]

{ #category : 'testing' }
SycNewClassCreationCommand >> canBeExecuted [

	^ context lastSelectedClass isTrait not
]

{ #category : 'accessing' }
SycNewClassCreationCommand >> packageNames [
	"Answer a <Collection> of <Symbol> of the packages available to the new class"
	
	^ PackageOrganizer default packageNames
]

{ #category : 'execution' }
SycNewClassCreationCommand >> prepareFullExecution [
	
	refactoringScopes := context refactoringScopes.
	targetClass := context lastSelectedClass.

]
