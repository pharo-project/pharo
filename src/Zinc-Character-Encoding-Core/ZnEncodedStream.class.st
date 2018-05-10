"
I am ZnEncodedStream, an abstract support class for read and write streams on an encoded binary stream.

Part of Zinc HTTP Components
"
Class {
	#name : #ZnEncodedStream,
	#superclass : #Object,
	#instVars : [
		'stream',
		'encoder'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #accessing }
ZnEncodedStream class >> defaultEncoder [
	^ ZnCharacterEncoder utf8
]

{ #category : #'instance creation' }
ZnEncodedStream class >> on: wrappedStream [
	^ self new
		on: wrappedStream;
		yourself
]

{ #category : #'instance creation' }
ZnEncodedStream class >> on: wrappedStream encoding: encoding [
	^ self new
		on: wrappedStream;
		encoding: encoding;
		yourself
]

{ #category : #'initialize-release' }
ZnEncodedStream >> close [
	stream close
]

{ #category : #testing }
ZnEncodedStream >> closed [
	^ stream closed
]

{ #category : #accessing }
ZnEncodedStream >> encoder [
	^ encoder ifNil: [ encoder := self class defaultEncoder ]
]

{ #category : #'initialize-release' }
ZnEncodedStream >> encoder: characterEncoder [
	encoder := characterEncoder
]

{ #category : #'initialize-release' }
ZnEncodedStream >> encoding: encoding [
	encoder := encoding asZnCharacterEncoder
]

{ #category : #accessing }
ZnEncodedStream >> flush [
	^ stream flush
]

{ #category : #testing }
ZnEncodedStream >> isStream [
  ^ true
]

{ #category : #'initialize-release' }
ZnEncodedStream >> on: wrappedStream [
	stream := wrappedStream
]

{ #category : #accessing }
ZnEncodedStream >> position [
	^ stream position
]

{ #category : #accessing }
ZnEncodedStream >> position: aPosition [
	stream position: aPosition
]

{ #category : #accessing }
ZnEncodedStream >> setToEnd [
	stream setToEnd
]

{ #category : #accessing }
ZnEncodedStream >> size [
	^ stream size
]

{ #category : #accessing }
ZnEncodedStream >> truncate [
	stream truncate
]

{ #category : #accessing }
ZnEncodedStream >> truncate: anInteger [ 
	stream truncate: anInteger
]

{ #category : #accessing }
ZnEncodedStream >> wrappedStream [
	^ stream
]
