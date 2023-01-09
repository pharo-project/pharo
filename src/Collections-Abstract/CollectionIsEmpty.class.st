"
I am CollectionIsEmpty, an exception indicating that an operation was attempted 
on an empty collection where that does not make sense.
I am an Error and thus an Exception.

The collection that is empty is in my inherited signaler instance variable.

"
Class {
	#name : #CollectionIsEmpty,
	#superclass : #Error,
	#category : #'Collections-Abstract-Errors'
}

{ #category : #'instance creation' }
CollectionIsEmpty class >> signalWith: aCollection [
	^ self new
		collection: aCollection;
		signal
]

{ #category : #private }
CollectionIsEmpty >> collection [
	"Return the collection where something is not found in"

	^ self signaler
]

{ #category : #accessing }
CollectionIsEmpty >> collection: aCollection [
	"Set the collection where something is not found in"

	self signaler: aCollection
]

{ #category : #accessing }
CollectionIsEmpty >> messageText [
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"

	^ messageText ifNil: [ messageText := self standardMessageText ]
]

{ #category : #private }
CollectionIsEmpty >> standardMessageText [
	"Generate a standard textual description"

	^ String streamContents: [ :stream |
		stream print: self collection.
		stream << ' is empty' ]
]
