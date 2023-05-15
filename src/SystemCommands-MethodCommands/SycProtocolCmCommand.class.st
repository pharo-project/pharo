"
This class is an extension so we can mix command of Commander2 with Commander.
"
Class {
	#name : #SycProtocolCmCommand,
	#superclass : #SycCmCommand,
	#instVars : [
		'methodGroups'
	],
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #execution }
SycProtocolCmCommand class >> activationStrategy [
	^ SycProtocolMenuActivation
]

{ #category : #preparation }
SycProtocolCmCommand >> prepareFullExecution [
	super prepareFullExecution.
	methodGroups := context selectedMethodGroups
]
