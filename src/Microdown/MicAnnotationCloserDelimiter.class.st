Class {
	#name : #MicAnnotationCloserDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-ModelInline'
}

{ #category : #accessing }
MicAnnotationCloserDelimiter class >> associatedInlineBlock [
	^ MicAnnotationInlineBlock 
]

{ #category : #accessing }
MicAnnotationCloserDelimiter class >> isCloser [
	^ true
]

{ #category : #accessing }
MicAnnotationCloserDelimiter class >> isOpener [
	^ false
]

{ #category : #accessing }
MicAnnotationCloserDelimiter class >> markup [
	^ '?>'
]

{ #category : #accessing }
MicAnnotationCloserDelimiter class >> type [
	^ #annotation
]
