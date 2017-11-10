"
Common superclass for metacello writers
"
Class {
	#name : #MCWriter,
	#superclass : #Object,
	#instVars : [
		'stream'
	],
	#category : #Monticello-Storing
}

{ #category : #accessing }
MCWriter class >> extension [
	^ self readerClass extension
]

{ #category : #writing }
MCWriter class >> on: aStream [
	^ self new stream: aStream
]

{ #category : #accessing }
MCWriter class >> readerClass [
	^ self subclassResponsibility 
]

{ #category : #accessing }
MCWriter >> stream [
	^ stream
]

{ #category : #accessing }
MCWriter >> stream: aStream [
	stream := aStream
]
