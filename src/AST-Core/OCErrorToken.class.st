"
I'm a scanned error.
I can have multiple causes.
"
Class {
	#name : 'OCErrorToken',
	#superclass : 'OCValueToken',
	#instVars : [
		'cause',
		'location'
	],
	#category : 'AST-Core-Tokens',
	#package : 'AST-Core',
	#tag : 'Tokens'
}

{ #category : 'instance creation' }
OCErrorToken class >> value: value start: tokenStart cause: errorCause location: thePosition [
	^ self new
		value: value
			start: tokenStart
			cause: errorCause
			location: thePosition;
		yourself
]

{ #category : 'accessing' }
OCErrorToken >> cause [
	^ cause
]

{ #category : 'testing' }
OCErrorToken >> isError [
	^true
]

{ #category : 'accessing' }
OCErrorToken >> location [
	^ location
]

{ #category : 'accessing' }
OCErrorToken >> location: anInteger [
	location := anInteger
]

{ #category : 'accessing' }
OCErrorToken >> value: theValue start: tokenStart cause: errorCause location: errorPosition [
	self value: theValue start: tokenStart.
	location := errorPosition.
	cause := errorCause
]
