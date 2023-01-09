"
I am NotFound, an exception to indicate that something is not found in a collection.
I am an Error and thus Exception.

Typically, the thing not found is in my object instance variable.
The collection where this thing was not found is in my inherited signaler instance variable.

[ NotFound signalFor: 10 in: #(1 2 3) ] on: NotFound do: [ :exception | exception ]
"
Class {
	#name : #NotFound,
	#superclass : #Error,
	#instVars : [
		'object'
	],
	#category : #'Collections-Abstract-Errors'
}

{ #category : #'instance creation' }
NotFound class >> signalFor: anObject [
	"Create and signal a NotFound exception for anObject in the default receiver."

	^ self new
		object: anObject;
		signal
]

{ #category : #'instance creation' }
NotFound class >> signalFor: anObject in: aCollection [
	"Create and signal a NotFound exception for anObject in aCollection."

	^ self new
		object: anObject;
		collection: aCollection;
		signal
]

{ #category : #accessing }
NotFound >> collection [
	"Return the collection where something is not found in"

	^ self signaler
]

{ #category : #accessing }
NotFound >> collection: aCollection [
	"Set the collection where something is not found in"

	self signaler: aCollection
]

{ #category : #accessing }
NotFound >> messageText [
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"

	^ messageText ifNil: [ messageText := self standardMessageText ]
]

{ #category : #accessing }
NotFound >> object [
	"Return the object that was not found"

	^ object
]

{ #category : #accessing }
NotFound >> object: anObject [
	"Set the object that was not found"

	object := anObject
]

{ #category : #private }
NotFound >> standardMessageText [
	"Generate a standard textual description"

	^ String streamContents: [ :stream |
		stream print: self object.
		stream << ' not found in '.
		stream print: self collection class ]
]
