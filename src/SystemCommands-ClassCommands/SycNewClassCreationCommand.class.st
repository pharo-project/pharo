"
I am a base class for commands which create new class.
They all needs new class name

Internal Representation and Key Implementation Points.

    Instance Variables
	newClassName:		<String>
"
Class {
	#name : #SycNewClassCreationCommand,
	#superclass : #SycSingleClassCommand,
	#instVars : [
		'newClassName'
	],
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #testing }
SycNewClassCreationCommand class >> isAbstract [
	^self = SycNewClassCreationCommand 
]

{ #category : #execution }
SycNewClassCreationCommand >> applyResultInContext: aToolContext [
	
	| newClass |
	newClass := aToolContext system classNamed: newClassName.
	
	aToolContext showClass: newClass
]

{ #category : #accessing }
SycNewClassCreationCommand >> newClassName [
	^ newClassName
]

{ #category : #accessing }
SycNewClassCreationCommand >> newClassName: anObject [
	newClassName := anObject
]

{ #category : #execution }
SycNewClassCreationCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	newClassName := UIManager default request: 'Name of new class'.	
	newClassName isEmptyOrNil | (newClassName = targetClass name) ifTrue: [ CmdCommandAborted signal].
	
]
