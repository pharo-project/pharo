"
I am a base class for commands which perform operations on collection of messages.

Messages are instances of SycMessageDescription which is a pair of selector and arguments.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	messages:		<Collection of<SycMessageDescription>>

"
Class {
	#name : 'SycMessageCommand',
	#superclass : 'CmdCommand',
	#instVars : [
		'messages'
	],
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'testing' }
SycMessageCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isMessageSelected | aToolContext isMethodSelected
]

{ #category : 'instance creation' }
SycMessageCommand class >> for: messages [
	^self new
		messages: messages
]

{ #category : 'instance creation' }
SycMessageCommand class >> forMethods: methods [
	^self for: (methods collect: [:each | SycMessageDescription ofMethod: each])
]

{ #category : 'testing' }
SycMessageCommand class >> isAbstract [
	^self = SycMessageCommand
]

{ #category : 'accessing' }
SycMessageCommand >> messages [
	^ messages
]

{ #category : 'accessing' }
SycMessageCommand >> messages: anObject [
	messages := anObject
]

{ #category : 'execution' }
SycMessageCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	messages := aToolContext selectedMessages
]

{ #category : 'accessing' }
SycMessageCommand >> selectedClass [
	"Answer the receiver's selected <Class>"

	^ self selectedMessage methodClass
]

{ #category : 'accessing' }
SycMessageCommand >> selectedMessage [
	"Answer a <SycMessageDescription> describing the selected message"

	^ self messages anyOne
]

{ #category : 'execution' }
SycMessageCommand >> selectedSelector [

	^ self selectedMessage selector
]
