Class {
	#name : #MicAnnotationOpenerDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-Parser'
}

{ #category : #accessing }
MicAnnotationOpenerDelimiter class >> associatedInlineBlock [
	^ MicAnnotationBlock 
]

{ #category : #accessing }
MicAnnotationOpenerDelimiter class >> isCloser [
	^ false
]

{ #category : #accessing }
MicAnnotationOpenerDelimiter class >> isOpener [
	^ true
]

{ #category : #accessing }
MicAnnotationOpenerDelimiter class >> markup [

	^ '?{'
]

{ #category : #accessing }
MicAnnotationOpenerDelimiter class >> type [
	^ #annotation
]
