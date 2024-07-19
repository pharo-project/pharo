"
I am a base class for commands which perform operations with single class
 
Internal Representation and Key Implementation Points.

    Instance Variables
	targetClass:		<Class>
"
Class {
	#name : 'SycSingleClassCommand',
	#superclass : 'CmdCommand',
	#instVars : [
		'targetClass'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'testing' }
SycSingleClassCommand class >> canBeExecutedInContext: aToolContext [

	^ (super canBeExecutedInContext: aToolContext) and: [
		  aToolContext isClassSelected ]
]

{ #category : 'testing' }
SycSingleClassCommand class >> isAbstract [
	^self = SycSingleClassCommand
]

{ #category : 'accessing' }
SycSingleClassCommand >> description [
	"Answer a <String> describing the receiver, useful for instance in menu items pop-ups"

	^ self class comment.
]

{ #category : 'execution' }
SycSingleClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	targetClass := aToolContext lastSelectedClass
]

{ #category : 'accessing' }
SycSingleClassCommand >> targetClass [
	^ targetClass
]

{ #category : 'accessing' }
SycSingleClassCommand >> targetClass: anObject [
	targetClass := anObject
]
