"
I am a base class for commands which create new class.
They all needs new class name

Internal Representation and Key Implementation Points.

    Instance Variables
	newClassName:		<String>
"
Class {
	#name : 'SycCmNewClassCreationCommand',
	#superclass : 'SycClassCmCommand',
	#instVars : [
		'newClassName'
	],
	#category : 'SystemCommands-ClassCommands-Creation',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Creation'
}

{ #category : 'execution' }
SycCmNewClassCreationCommand >> applyCommandResult [
	"Check if user cancelled while viewing changes"
	| newClass |
	
	(self class environment hasClassNamed: newClassName)
		ifFalse: [ ^ self ].
		
	newClass := context system classNamed: newClassName.
	context showClass: newClass
]

{ #category : 'testing' }
SycCmNewClassCreationCommand >> isApplicable [

	^ false
]

{ #category : 'preparation' }
SycCmNewClassCreationCommand >> prepareFullExecution [

	super prepareFullExecution.
	newClassName := UIManager default request: 'Name of new class'.
	newClassName isEmptyOrNil | (newClassName = targetClass name) ifTrue: [ CmdCommandAborted signal]
]
