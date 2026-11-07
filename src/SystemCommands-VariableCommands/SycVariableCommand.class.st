"
I am a base class for command which perform operations with given collection of variables.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	variables:		<Collection of<Variable>>

"
Class {
	#name : 'SycVariableCommand',
	#superclass : 'ClyCommand',
	#instVars : [
		'variables',
		'toolContext'
	],
	#category : 'SystemCommands-VariableCommands',
	#package : 'SystemCommands-VariableCommands'
}

{ #category : 'testing' }
SycVariableCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isVariableSelected
]

{ #category : 'testing' }
SycVariableCommand class >> isAbstract [
	^self = SycVariableCommand
]

{ #category : 'accessing' }
SycVariableCommand >> variables [
	^ variables
]

{ #category : 'accessing' }
SycVariableCommand >> variables: anObject [
	variables := anObject
]
