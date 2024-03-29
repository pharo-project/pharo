"
I am a command to copy given class using given name.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newClassName:		<String>
	originalClass:		<Class>
	resultClass:		<Class>

"
Class {
	#name : 'SycDuplicateClassCommand',
	#superclass : 'CmdCommand',
	#instVars : [
		'originalClass',
		'newClassName',
		'resultClass'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'testing' }
SycDuplicateClassCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isClassSelected
]

{ #category : 'execution' }
SycDuplicateClassCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showClass: resultClass
]

{ #category : 'accessing' }
SycDuplicateClassCommand >> defaultMenuIconName [
	^#smallCopy
]

{ #category : 'accessing' }
SycDuplicateClassCommand >> defaultMenuItemName [
	^'Duplicate'
]

{ #category : 'execution' }
SycDuplicateClassCommand >> execute [

	resultClass := originalClass duplicateClassWithNewName: newClassName
]

{ #category : 'accessing' }
SycDuplicateClassCommand >> newClassName [
	^ newClassName
]

{ #category : 'accessing' }
SycDuplicateClassCommand >> newClassName: anObject [
	newClassName := anObject
]

{ #category : 'accessing' }
SycDuplicateClassCommand >> originalClass [
	^ originalClass
]

{ #category : 'accessing' }
SycDuplicateClassCommand >> originalClass: anObject [
	originalClass := anObject
]

{ #category : 'execution' }
SycDuplicateClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	originalClass := aToolContext lastSelectedClass.

	newClassName := self morphicUIManager request: 'Input new class name' initialAnswer: originalClass name.

	newClassName isEmptyOrNil | (newClassName = originalClass name)
		ifTrue: [ CmdCommandAborted signal ]
]
