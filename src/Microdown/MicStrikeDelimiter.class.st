"
I'm the delimiter for strikethrough format.
"
Class {
	#name : #MicStrikeDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-ModelInline'
}

{ #category : #accessing }
MicStrikeDelimiter class >> associatedInlineBlock [
	^ MicStrikeInlineBlock 
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
