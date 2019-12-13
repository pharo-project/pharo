"
I represent an error that appears when a data cannot be unpacked (materialized).

Collaborators: GTEventUnpacking

"
Class {
	#name : #GTEventUnpackingError,
	#superclass : #GTEventRecorderError,
	#instVars : [
		'data',
		'errors'
	],
	#category : #'GT-EventRecorder-Exceptions'
}

{ #category : #'instance creation' }
GTEventUnpackingError class >> signal: aString data: aByteArray errors: aCollection [ 
	^ self new
			data: aByteArray;
			errors: aCollection;
			signal: aString;
			yourself
]

{ #category : #accessing }
GTEventUnpackingError >> data [
	^ data
]

{ #category : #accessing }
GTEventUnpackingError >> data: anObject [
	data := anObject
]

{ #category : #accessing }
GTEventUnpackingError >> errors [
	^ errors
]

{ #category : #accessing }
GTEventUnpackingError >> errors: anObject [
	errors := anObject
]

{ #category : #private }
GTEventUnpackingError >> isResumable [
	"Determine whether an exception is resumable."
	^true
]
