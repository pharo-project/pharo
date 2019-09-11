"
I am a command to copy given class using given name.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newClassName:		<String>
	originalClass:		<Class>
	resultClass:		<Class>

"
Class {
	#name : #SycCopyClassCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'originalClass',
		'newClassName',
		'resultClass'
	],
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #testing }
SycCopyClassCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isClassSelected
]

{ #category : #execution }
SycCopyClassCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showClass: resultClass
]

{ #category : #accessing }
SycCopyClassCommand >> defaultMenuIconName [
	^#smallCopy
]

{ #category : #accessing }
SycCopyClassCommand >> defaultMenuItemName [
	^'Copy'
]

{ #category : #execution }
SycCopyClassCommand >> execute [
	
	resultClass := originalClass duplicateClassWithNewName: newClassName
]

{ #category : #accessing }
SycCopyClassCommand >> newClassName [
	^ newClassName
]

{ #category : #accessing }
SycCopyClassCommand >> newClassName: anObject [
	newClassName := anObject
]

{ #category : #accessing }
SycCopyClassCommand >> originalClass [
	^ originalClass
]

{ #category : #accessing }
SycCopyClassCommand >> originalClass: anObject [
	originalClass := anObject
]

{ #category : #execution }
SycCopyClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	originalClass := aToolContext lastSelectedClass.
	
	newClassName := UIManager default request: 'Input new class name' initialAnswer: originalClass name.
	
	newClassName isEmptyOrNil | (newClassName = originalClass name) 
		ifTrue: [ CmdCommandAborted signal ]
]
