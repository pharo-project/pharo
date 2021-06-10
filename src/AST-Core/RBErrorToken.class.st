"
I'm a scanned error.
I can have multiple causes.
"
Class {
	#name : #RBErrorToken,
	#superclass : #RBValueToken,
	#instVars : [
		'cause',
		'location'
	],
	#category : #'AST-Core-Tokens'
}

{ #category : #'instance creation' }
RBErrorToken class >> value: value start: tokenStart cause: errorCause location: thePosition [
	^ self new
		value: value
			start: tokenStart
			cause: errorCause
			location: thePosition;
		yourself
]

{ #category : #accessing }
RBErrorToken >> cause [
	^ cause
]

{ #category : #testing }
RBErrorToken >> isError [
	^true
]

{ #category : #accessing }
RBErrorToken >> location [
	^ location
]

{ #category : #accessing }
RBErrorToken >> value: theValue start: tokenStart cause: errorCause location: errorPosition [
	self value: theValue start: tokenStart.
	location := errorPosition.
	cause := errorCause
]
