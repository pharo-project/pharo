Class {
	#name : #MicEscapeDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-ModelInline'
}

{ #category : #accessing }
MicEscapeDelimiter class >> associatedInlineBlock [
	^ nil
]

{ #category : #accessing }
MicEscapeDelimiter class >> isCloser [
	^ false
]

{ #category : #accessing }
MicEscapeDelimiter class >> isOpener [
	^ false
]

{ #category : #accessing }
MicEscapeDelimiter class >> markup [
	^ '\'
]

{ #category : #accessing }
MicEscapeDelimiter class >> type [
	^ #escape
]
