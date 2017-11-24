"
I am delivered when user drop something into the window (of course, if there's a drag and drop support by underlaying platform).
"
Class {
	#name : #OSWindowDropEvent,
	#superclass : #OSWindowEvent,
	#instVars : [
		'filePath',
		'timestamp'
	],
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSWindowDropEvent >> accept: aVisitor [
	^ aVisitor visitWindowDropEvent: self
]

{ #category : #accessing }
OSWindowDropEvent >> filePath [
	^ filePath
]

{ #category : #accessing }
OSWindowDropEvent >> filePath: anObject [
	filePath := anObject
]

{ #category : #printing }
OSWindowDropEvent >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPutAll: ' filePath: ''';
		nextPutAll: filePath;
		nextPut: $'
]

{ #category : #accessing }
OSWindowDropEvent >> timestamp [
	^ timestamp
]

{ #category : #accessing }
OSWindowDropEvent >> timestamp: anObject [
	timestamp := anObject
]
