"
ZnClientTransactionEvent is a ZnClientLogEvent that notes the completion of a client side HTTP transaction (request/response).

Instance Variables
	request: a ZnRequest
	requestDuration:	 milliseconds
	response:	a ZnResponse
	responseDuration: milliseconds
	
Note that this can be quite large since it includes the entities transferred. See ZnSimplifiedClientTransactionEvent for a more compact representation.
"
Class {
	#name : #ZnClientTransactionEvent,
	#superclass : #ZnClientLogEvent,
	#instVars : [
		'request',
		'response',
		'requestDuration',
		'responseDuration'
	],
	#category : #'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnClientTransactionEvent >> duration [
	^ requestDuration + responseDuration
]

{ #category : #printing }
ZnClientTransactionEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream space.
	request requestLine printMethodAndUriOn: stream.
	stream space; print: response code.
	response hasEntity
		ifTrue: [ 
			stream space; print: response entity contentLength; nextPut: $B ].
	stream space; print: self duration; nextPutAll: 'ms'
]

{ #category : #accessing }
ZnClientTransactionEvent >> request [
	^ request
]

{ #category : #accessing }
ZnClientTransactionEvent >> request: anObject [
	request := anObject
]

{ #category : #accessing }
ZnClientTransactionEvent >> requestDuration [
	^ requestDuration
]

{ #category : #accessing }
ZnClientTransactionEvent >> requestDuration: anObject [
	requestDuration := anObject
]

{ #category : #accessing }
ZnClientTransactionEvent >> response [
	^ response
]

{ #category : #accessing }
ZnClientTransactionEvent >> response: anObject [
	response := anObject
]

{ #category : #accessing }
ZnClientTransactionEvent >> responseDuration [
	^ responseDuration
]

{ #category : #accessing }
ZnClientTransactionEvent >> responseDuration: anObject [
	responseDuration := anObject
]
