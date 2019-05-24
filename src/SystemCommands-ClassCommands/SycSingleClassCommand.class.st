"
I am a base class for commands which perform operations with single class
 
Internal Representation and Key Implementation Points.

    Instance Variables
	targetClass:		<Class>
"
Class {
	#name : #SycSingleClassCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'targetClass'
	],
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #testing }
SycSingleClassCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isClassSelected 
]

{ #category : #testing }
SycSingleClassCommand class >> isAbstract [
	^self = SycSingleClassCommand 
]

{ #category : #execution }
SycSingleClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	targetClass := aToolContext lastSelectedClass.
]

{ #category : #accessing }
SycSingleClassCommand >> targetClass [
	^ targetClass
]

{ #category : #accessing }
SycSingleClassCommand >> targetClass: anObject [
	targetClass := anObject
]
