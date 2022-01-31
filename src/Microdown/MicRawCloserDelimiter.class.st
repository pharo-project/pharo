"
I am the closer delimiter for raw.
"
Class {
	#name : #MicRawCloserDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-Parser'
}

{ #category : #accessing }
MicRawCloserDelimiter class >> associatedInlineBlock [
	^ MicRawBlock 
]

{ #category : #accessing }
MicRawCloserDelimiter class >> isCloser [
	^ true
]

{ #category : #accessing }
MicRawCloserDelimiter class >> isOpener [
	^ false
]

{ #category : #accessing }
MicRawCloserDelimiter class >> markup [
	^ '}}'
]

{ #category : #accessing }
MicRawCloserDelimiter class >> type [
	^ #raw
]
