"
ZnUnknownHttpMethod is signalled when an unknown/unsupported HTTP method is found while parsing a request.

I am resumable to allow non-standard methods.

Part of Zinc HTTP Components. 
"
Class {
	#name : #ZnUnknownHttpMethod,
	#superclass : #ZnParseError,
	#instVars : [
		'method'
	],
	#category : #'Zinc-HTTP-Exceptions'
}

{ #category : #'instance creation' }
ZnUnknownHttpMethod class >> method: method [
	^ self new
		method: method;
		yourself
]

{ #category : #testing }
ZnUnknownHttpMethod >> isResumable [
	^ true
]

{ #category : #accessing }
ZnUnknownHttpMethod >> method [
	^ method
]

{ #category : #accessing }
ZnUnknownHttpMethod >> method: anObject [
	method := anObject
]
