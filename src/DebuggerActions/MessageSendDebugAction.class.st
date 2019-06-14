"
A MessageSendDebugAction is an action that sends a message to a receiver with the right amount of arguments. 

To specify the receiver subclasses have to override the method #receiver. Furthermoe subclasses must specify an id. The selector is optional as the id will be used if no selector is present/


Instance Variables
	id:		<Object>
	selector:		<Object>

id
	- xxxxx

selector
	- xxxxx

"
Class {
	#name : #MessageSendDebugAction,
	#superclass : #DebugAction,
	#instVars : [
		'id',
		'selector',
		'help'
	],
	#category : #'Debugger-Actions-Actions'
}

{ #category : #accessing }
MessageSendDebugAction >> arguments [

	^ #()
]

{ #category : #actions }
MessageSendDebugAction >> executeAction [
	
	self receiver 
		perform: self selector
		withEnoughArguments: self arguments
]

{ #category : #accessing }
MessageSendDebugAction >> help [
	^ help
]

{ #category : #accessing }
MessageSendDebugAction >> help: anObject [
	help := anObject
]

{ #category : #accessing }
MessageSendDebugAction >> id [

	^ id
]

{ #category : #accessing }
MessageSendDebugAction >> id: aSymbol [
	
	id := aSymbol 
]

{ #category : #accessing }
MessageSendDebugAction >> receiver [

	^ nil
]

{ #category : #accessing }
MessageSendDebugAction >> selector [

	^ selector ifNil: [ self id ]
]

{ #category : #accessing }
MessageSendDebugAction >> selector: aSymbol [

	selector := aSymbol
]
