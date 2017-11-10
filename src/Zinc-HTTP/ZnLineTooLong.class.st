"
ZnLineTooLong is signalled when an HTTP CRLF terminated input line exceeds a predefined limit.

Part of Zinc HTTP Components. 
"
Class {
	#name : #ZnLineTooLong,
	#superclass : #ZnParseError,
	#instVars : [
		'limit'
	],
	#category : #Zinc-HTTP-Exceptions
}

{ #category : #'instance creation' }
ZnLineTooLong class >> limit: limit [
	^ self new 
		limit: limit; 
		yourself
]

{ #category : #accessing }
ZnLineTooLong >> limit [
	^ limit
]

{ #category : #accessing }
ZnLineTooLong >> limit: anObject [
	limit := anObject
]
