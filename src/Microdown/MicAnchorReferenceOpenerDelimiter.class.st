Class {
	#name : #MicAnchorReferenceOpenerDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-ModelInline'
}

{ #category : #accessing }
MicAnchorReferenceOpenerDelimiter class >> associatedInlineBlock [
	^ MicAnchorReferenceInlineBlock 
]

{ #category : #accessing }
MicAnchorReferenceOpenerDelimiter class >> isCloser [
	^ false
]

{ #category : #accessing }
MicAnchorReferenceOpenerDelimiter class >> isOpener [
	^ true
]

{ #category : #accessing }
MicAnchorReferenceOpenerDelimiter class >> markup [
	^ '*@'
]

{ #category : #accessing }
MicAnchorReferenceOpenerDelimiter class >> type [
	^ #anchorReference
]
