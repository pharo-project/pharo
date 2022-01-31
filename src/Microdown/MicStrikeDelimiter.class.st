"
I'm the delimiter for strikethrough format.
"
Class {
	#name : #MicStrikeDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-Parser'
}

{ #category : #accessing }
MicStrikeDelimiter class >> associatedInlineBlock [
	^ MicStrikeFormatBlock 
]

{ #category : #accessing }
MicStrikeDelimiter class >> isCloser [
	^ true
]

{ #category : #accessing }
MicStrikeDelimiter class >> isOpener [
	^ true
]

{ #category : #accessing }
MicStrikeDelimiter class >> markup [
	^ '~'
]

{ #category : #accessing }
MicStrikeDelimiter class >> type [
	^ #strike
]
