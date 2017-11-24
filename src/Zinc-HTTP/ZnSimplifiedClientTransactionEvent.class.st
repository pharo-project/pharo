"
ZnSimplifiedClientTransactionEvent is a ZnClientLogEvent that notes the completion of a client side HTTP transaction (request/response). Only a limited number of simpler data is held. ZnClientTransactionEvent contains much more detailed data.

Instance Variables
	method: HTTP verb
	url: ZnUrl requested
	response:	numeric response code
	size: number of bytes in response
	duration: milliseconds

ZnSimplifiedClientTransactionEvent is ideal for classic HTTP logging with little overhead through its default string representation.
"
Class {
	#name : #ZnSimplifiedClientTransactionEvent,
	#superclass : #ZnClientLogEvent,
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
ZnSimplifiedClientTransactionEvent >> duration [
	^ duration
]

{ #category : #initialize }
ZnSimplifiedClientTransactionEvent >> initialize [
	super initialize.
	duration := 0
]

{ #category : #accessing }
ZnSimplifiedClientTransactionEvent >> method [
	^ method
]

{ #category : #printing }
ZnSimplifiedClientTransactionEvent >> printContentsOn: stream [
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
ZnSimplifiedClientTransactionEvent >> request: aZnRequest [
	method := aZnRequest method.
	url := aZnRequest url
]

{ #category : #initialize }
ZnSimplifiedClientTransactionEvent >> requestDuration: requestDuration [
	duration := duration + requestDuration 
]

{ #category : #initialize }
ZnSimplifiedClientTransactionEvent >> response: aZnResponse [
	response := aZnResponse code.
	aZnResponse hasEntity
		ifTrue: [ size := aZnResponse entity contentLength ]
]

{ #category : #accessing }
ZnSimplifiedClientTransactionEvent >> responseCode [
	^ response
]

{ #category : #initialize }
ZnSimplifiedClientTransactionEvent >> responseDuration: responseDuration [
	duration := duration + responseDuration 
]

{ #category : #accessing }
ZnSimplifiedClientTransactionEvent >> size [
	^ size
]

{ #category : #accessing }
ZnSimplifiedClientTransactionEvent >> url [
	^ url
]
