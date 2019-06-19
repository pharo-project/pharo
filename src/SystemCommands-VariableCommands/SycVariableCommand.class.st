"
I am a base class for command which perform operations with given collection of variables.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	variables:		<Collection of<Variable>>

"
Class {
	#name : #SycVariableCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'variables'
	],
	#category : #'SystemCommands-VariableCommands'
}

{ #category : #testing }
SycVariableCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isVariableSelected
]

{ #category : #testing }
SycVariableCommand class >> isAbstract [
	^self = SycVariableCommand 
]

{ #category : #execution }
SycVariableCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	variables := aToolContext selectedVariables
]

{ #category : #accessing }
SycVariableCommand >> variables [
	^ variables
]

{ #category : #accessing }
SycVariableCommand >> variables: anObject [
	variables := anObject
]
