"
I am the delimiter for monospace/inline code format.
"
Class {
	#name : #MicMonospaceDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-Parser'
}

{ #category : #accessing }
MicMonospaceDelimiter class >> associatedInlineBlock [
	^ MicMonospaceFormatBlock 
]

{ #category : #accessing }
MicMonospaceDelimiter class >> isCloser [
	^ true
]

{ #category : #accessing }
MicMonospaceDelimiter class >> isOpener [
	^ true
]

{ #category : #accessing }
MicMonospaceDelimiter class >> markup [
	^ '`'
]

{ #category : #accessing }
MicMonospaceDelimiter class >> type [
	^ #monospace
]
