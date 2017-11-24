"
A test mock for socket streams
"
Class {
	#name : #MockSocketStream,
	#superclass : #Stream,
	#instVars : [
		'atEnd',
		'inStream',
		'outStream'
	],
	#category : #'Network-Tests-Kernel'
}

{ #category : #'instance creation' }
MockSocketStream class >> on: socket [
	^self basicNew initialize
]

{ #category : #testing }
MockSocketStream >> atEnd [
	^self inStream atEnd.
]

{ #category : #accessing }
MockSocketStream >> atEnd: aBoolean [
	atEnd := aBoolean.
]

{ #category : #accessing }
MockSocketStream >> inStream [
	^inStream
]

{ #category : #initialization }
MockSocketStream >> initialize [
	super initialize.
	self resetInStream.
	self resetOutStream.
]

{ #category : #'stream in' }
MockSocketStream >> nextLine [
	^self nextLineCrLf
]

{ #category : #'stream in' }
MockSocketStream >> nextLineCrLf [
	^(self upToAll: String crlf).
]

{ #category : #accessing }
MockSocketStream >> outStream [
	^outStream
]

{ #category : #'stream in' }
MockSocketStream >> resetInStream [
	inStream := String new writeStream.
]

{ #category : #'stream out' }
MockSocketStream >> resetOutStream [
	outStream := String new writeStream.
]

{ #category : #'stream out' }
MockSocketStream >> sendCommand: aString [
	self outStream
		nextPutAll: aString;
		nextPutAll: String crlf.
]

{ #category : #'stream in' }
MockSocketStream >> upToAll: delims [
	^self inStream upToAll: delims.
]
