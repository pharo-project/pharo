"
ZnServerTransactionEvent is a ZnServerLogEvent that notes the completion of a server side HTTP transaction (request/response).

Instance Variables
	request: a ZnRequest
	response:	a ZnResponse
	timing: a ZnServerTransactionTiming

Note that this can be quite large since it includes the entities transferred. See ZnSimplifiedServerTransactionEvent for a more compact representation.
"
Class {
	#name : #ZnServerTransactionEvent,
	#superclass : #ZnServerLogEvent,
	#instVars : [
		'request',
		'response',
		'timing'
	],
	#category : 'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnServerTransactionEvent >> duration [
	^ timing totalDuration
]

{ #category : #printing }
ZnServerTransactionEvent >> printContentsOn: stream [
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
ZnServerTransactionEvent >> request [
	^ request
]

{ #category : #accessing }
ZnServerTransactionEvent >> request: anObject [
	request := anObject
]

{ #category : #accessing }
ZnServerTransactionEvent >> response [
	^ response
]

{ #category : #accessing }
ZnServerTransactionEvent >> response: anObject [
	response := anObject
]

{ #category : #accessing }
ZnServerTransactionEvent >> timing [
	^ timing
]

{ #category : #accessing }
ZnServerTransactionEvent >> timing: anObject [
	timing := anObject
]
