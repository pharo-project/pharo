"
I'm the base class for converting some data from a MIME type to another. Subclasses should implement #mimeDecode and #mimeEncode.
"
Class {
	#name : #MimeConverter,
	#superclass : #Object,
	#instVars : [
		'dataStream',
		'mimeStream'
	],
	#category : #'Network-MIME-Converter'
}

{ #category : #convenience }
MimeConverter class >> forEncoding: encodingString [
	"Answer a converter class for the given encoding or nil if unknown"
	encodingString ifNil: [^nil].
	^ encodingString asLowercase caseOf: 
		{ ['base64'] -> [Base64MimeConverter].
		  ['quoted-printable'] -> [QuotedPrintableMimeConverter]}
		otherwise: [].

]

{ #category : #convenience }
MimeConverter class >> mimeDecode: aStringOrStream as: contentsClass [
	^ contentsClass streamContents: [:out |
		self mimeDecode: aStringOrStream to: out]
]

{ #category : #convenience }
MimeConverter class >> mimeDecode: aStringOrStream to: outStream [ 
	self new
		mimeStream: (aStringOrStream isStream 
				ifTrue: [ aStringOrStream ]
				ifFalse: [ aStringOrStream readStream ]);
		dataStream: outStream;
		mimeDecode
]

{ #category : #convenience }
MimeConverter class >> mimeEncode: aCollectionOrStream [
	^ String streamContents: [:out |
		self mimeEncode: aCollectionOrStream to: out]
]

{ #category : #convenience }
MimeConverter class >> mimeEncode: aCollectionOrStream to: outStream [ 
	self new
		dataStream: (aCollectionOrStream isStream 
				ifTrue: [ aCollectionOrStream ]
				ifFalse: [ aCollectionOrStream readStream ]);
		mimeStream: outStream;
		mimeEncode
]

{ #category : #accessing }
MimeConverter >> dataStream [
	^dataStream
]

{ #category : #accessing }
MimeConverter >> dataStream: anObject [
	dataStream := anObject
]

{ #category : #conversion }
MimeConverter >> mimeDecode [
	"Do conversion reading from mimeStream writing to dataStream"

	self subclassResponsibility
]

{ #category : #conversion }
MimeConverter >> mimeEncode [
	"Do conversion reading from dataStream writing to mimeStream"

	self subclassResponsibility
]

{ #category : #accessing }
MimeConverter >> mimeStream [
	^mimeStream
]

{ #category : #accessing }
MimeConverter >> mimeStream: anObject [
	mimeStream := anObject
]
