"
ZnUnexpectedContentType is signalled when a different content type (mime type) was expected.

I hold the expected and actual content types.

Part of Zinc HTTP Components. 
"
Class {
	#name : #ZnUnexpectedContentType,
	#superclass : #Error,
	#instVars : [
		'expectedContentType',
		'actualContentType'
	],
	#category : #Zinc-HTTP-Exceptions
}

{ #category : #'instance creation' }
ZnUnexpectedContentType class >> expected: expectedMimeType actual: actualMimeType [
	^ self new
		expectedContentType: expectedMimeType;
		actualContentType: actualMimeType;
		yourself
]

{ #category : #accessing }
ZnUnexpectedContentType >> actualContentType [
	^ actualContentType
]

{ #category : #accessing }
ZnUnexpectedContentType >> actualContentType: anObject [
	actualContentType := anObject
]

{ #category : #accessing }
ZnUnexpectedContentType >> expectedContentType [
	^ expectedContentType
]

{ #category : #accessing }
ZnUnexpectedContentType >> expectedContentType: anObject [
	expectedContentType := anObject
]

{ #category : #accessing }
ZnUnexpectedContentType >> messageText [
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"
	
	^ messageText ifNil: [ messageText := self standardMessageText ]
]

{ #category : #accessing }
ZnUnexpectedContentType >> standardMessageText [
	"Generate a standard textual description"
	
	^ String streamContents: [ :stream |
		self expectedContentType ifNotNil: [ :contentType |
			stream << 'expected '; print: contentType ].
		self actualContentType ifNotNil: [ :contentType |
			self expectedContentType ifNotNil: [ stream space ].
			stream << 'actual '; print: contentType ] ]
]
