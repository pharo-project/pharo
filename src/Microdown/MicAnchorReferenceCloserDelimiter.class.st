Class {
	#name : #MicAnchorReferenceCloserDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-ModelInline'
}

{ #category : #accessing }
MicAnchorReferenceCloserDelimiter class >> associatedInlineBlock [
	^ MicAnchorReferenceInlineBlock 
]

{ #category : #accessing }
MicAnchorReferenceCloserDelimiter class >> isCloser [
	^ true
]

{ #category : #accessing }
MicAnchorReferenceCloserDelimiter class >> isOpener [
	^ false
]

{ #category : #accessing }
MicAnchorReferenceCloserDelimiter class >> markup [
	^ '@*'
]

{ #category : #accessing }
MicAnchorReferenceCloserDelimiter class >> type [
	^ #anchorReference 
]
