"
I am a basic filtering stream that will wrap every object put in the stream in a block, before sending it to my decoree.

"
Class {
	#name : #FilteringStream,
	#superclass : #DecoratorStream,
	#instVars : [
		'filter'
	],
	#category : #'Collections-Streams-Base'
}

{ #category : #'instance creation' }
FilteringStream class >> on: aStream [
	self error: 'FilteringStreams are created with #on:with:'
]

{ #category : #'instance creation' }
FilteringStream class >> on: aStream with: aBlock [
	^ self basicNew
		on: aStream;
		filter: aBlock;
		yourself.
]

{ #category : #accessing }
FilteringStream >> filter [
	^ filter
]

{ #category : #accessing }
FilteringStream >> filter: anObject [
	filter := anObject
]

{ #category : #accessing }
FilteringStream >> nextPut: anObject [ 
	"Insert the argument, anObject, as the next object accessible by the 
	receiver. Answer anObject."
	
	super nextPut: (filter value: anObject).
	^anObject 
]
