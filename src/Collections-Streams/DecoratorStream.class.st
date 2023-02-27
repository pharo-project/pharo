"
I am a basic decorator stream that will wrap a stream and forward messages to my decoree.

Subclass me to write a decorator that reuses some methods.
"
Class {
	#name : #DecoratorStream,
	#superclass : #Stream,
	#instVars : [
		'decoratedStream'
	],
	#category : #'Collections-Streams-Base'
}

{ #category : #'instance creation' }
DecoratorStream class >> on: aStream [

	^ self basicNew
		on: aStream;
		yourself
]

{ #category : #testing }
DecoratorStream >> atEnd [
	^ decoratedStream atEnd
]

{ #category : #accessing }
DecoratorStream >> contents [
	"Answer all of the contents of the receiver."

	^decoratedStream contents
]

{ #category : #accessing }
DecoratorStream >> next [

	^ decoratedStream next
]

{ #category : #accessing }
DecoratorStream >> nextPut: anObject [
	"Insert the argument, anObject, as the next object accessible by the
	receiver. Answer anObject."

	decoratedStream nextPut: anObject.
	^anObject
]

{ #category : #'initialize - release' }
DecoratorStream >> on: aStream [

	self initialize.
	decoratedStream := aStream
]

{ #category : #accessing }
DecoratorStream >> peek [

	^ decoratedStream peek
]

{ #category : #decorating }
DecoratorStream >> position [

	^ decoratedStream position
]

{ #category : #storing }
DecoratorStream >> store: anObject [

	anObject storeOn: self
]
