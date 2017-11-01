"
I am ZnServerTransactionTiming.

I hold timing information related to the handling of an HTTP transaction by a server.

"
Class {
	#name : #ZnServerTransactionTiming,
	#superclass : #Object,
	#instVars : [
		'requestDuration',
		'handlerDuration',
		'responseDuration'
	],
	#category : 'Zinc-HTTP-Logging'
}

{ #category : #accessing }
ZnServerTransactionTiming >> handlerDuration [
	^ handlerDuration
]

{ #category : #accessing }
ZnServerTransactionTiming >> handlerDuration: anObject [
	handlerDuration := anObject
]

{ #category : #initialize }
ZnServerTransactionTiming >> initialize [
	super initialize.
	requestDuration := handlerDuration := responseDuration := 0
]

{ #category : #accessing }
ZnServerTransactionTiming >> requestDuration [
	^ requestDuration
]

{ #category : #accessing }
ZnServerTransactionTiming >> requestDuration: anObject [
	requestDuration := anObject
]

{ #category : #accessing }
ZnServerTransactionTiming >> responseDuration [
	^ responseDuration
]

{ #category : #accessing }
ZnServerTransactionTiming >> responseDuration: anObject [
	responseDuration := anObject
]

{ #category : #accessing }
ZnServerTransactionTiming >> totalDuration [
	"requestDuration is unreliable because it contains idle wait time"
	
	^ handlerDuration + responseDuration 
]
