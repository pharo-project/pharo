"
An abstract base class for all text rendering commands.

A text rendered takes a stream (or list) of my (sub)instances and in that way it renders the text
"
Class {
	#name : #AthensTextDisplayCommand,
	#superclass : #Object,
	#instVars : [
		'next'
	],
	#category : 'Athens-Text'
}

{ #category : #accessing }
AthensTextDisplayCommand >> addLast: aCommand [
	self tail next: aCommand
]

{ #category : #accessing }
AthensTextDisplayCommand >> do: aBlock [
	| nn |
	nn := self.
	
	[ nn isNil ] whileFalse: [ 
		aBlock value: nn.
		nn := nn next. ]
]

{ #category : #accessing }
AthensTextDisplayCommand >> insert: aCommands [

	| nn |
	nn := next.
	next := aCommands.
	next tail next: nn
]

{ #category : #accessing }
AthensTextDisplayCommand >> next [

	^ next
]

{ #category : #accessing }
AthensTextDisplayCommand >> next: anObject [
	
	next := anObject
]

{ #category : #copying }
AthensTextDisplayCommand >> postCopy [
	"unlink the copy by default"
	next := nil
]

{ #category : #rendering }
AthensTextDisplayCommand >> renderOn: renderer [
	self shouldBeImplemented 
]

{ #category : #accessing }
AthensTextDisplayCommand >> tail [
	| nn |
	nn := self.
	[ nn next notNil ] whileTrue: [ nn := nn next ].
	^ nn
]
