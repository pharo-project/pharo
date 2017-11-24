"
ZnUnknownHttpStatusCode is signalled when an unknown/unsupported HTTP status code is found.

Part of Zinc HTTP Components. 
"
Class {
	#name : #ZnUnknownHttpStatusCode,
	#superclass : #ZnParseError,
	#instVars : [
		'code'
	],
	#category : #'Zinc-HTTP-Exceptions'
}

{ #category : #'instance creation' }
ZnUnknownHttpStatusCode class >> code: code [
	^ self new
		code: code;
		yourself
]

{ #category : #accessing }
ZnUnknownHttpStatusCode >> code [
	^ code
]

{ #category : #accessing }
ZnUnknownHttpStatusCode >> code: anObject [
	code := anObject
]
