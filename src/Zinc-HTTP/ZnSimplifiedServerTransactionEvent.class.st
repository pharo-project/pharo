"
ZnSimplifiedServerTransactionEvent is a ZnServerLogEvent that notes the completion of a served side HTTP transaction (request/response). Only a limited number of simpler data is held. ZnServerTransactionEvent contains much more detailed data.

Instance Variables
	method: HTTP verb
	url: ZnUrl requested
	response:	numeric response code
	size: number of bytes in response
	duration: milliseconds

ZnSimplifiedServerTransactionEvent is ideal for classic HTTP logging with little overhead through its default string representation.

"
Class {
	#name : #ZnSimplifiedServerTransactionEvent,
	#superclass : #ZnServerLogEvent,
	#instVars : [
		'method',
		'url',
		'response',
		'size',
		'duration'
	],
	#category : #'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnSimplifiedServerTransactionEvent >> duration [
	^ duration
]

{ #category : #accessing }
ZnSimplifiedServerTransactionEvent >> method [
	^ method
]

{ #category : #printing }
ZnSimplifiedServerTransactionEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream space; nextPutAll: method.
	stream space.
	url printPathQueryFragmentOn: stream.
	stream space; print: response.
	size ifNotNil: [ 
		stream space; print: size; nextPut: $B ].
	stream space; print: duration; nextPutAll: 'ms'
]

{ #category : #initialize }
ZnSimplifiedServerTransactionEvent >> request: aZnRequest [
	method := aZnRequest method.
	url := aZnRequest url
]

{ #category : #initialize }
ZnSimplifiedServerTransactionEvent >> response: aZnResponse [
	response := aZnResponse code.
	aZnResponse hasEntity
		ifTrue: [ size := aZnResponse entity contentLength ]
]

{ #category : #accessing }
ZnSimplifiedServerTransactionEvent >> responseCode [
	^ response
]

{ #category : #accessing }
ZnSimplifiedServerTransactionEvent >> size [
	^ size
]

{ #category : #initialize }
ZnSimplifiedServerTransactionEvent >> timing: timing [
	duration := timing totalDuration 
]

{ #category : #accessing }
ZnSimplifiedServerTransactionEvent >> url [
	^ url
]
