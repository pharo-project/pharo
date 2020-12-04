Class {
	#name : #MicAnnotationOpenerDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-ModelInline'
}

{ #category : #accessing }
MicAnnotationOpenerDelimiter class >> associatedInlineBlock [
	^ MicAnnotationInlineBlock 
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
	^ '<?'
]

{ #category : #accessing }
MicAnnotationOpenerDelimiter class >> type [
	^ #annotation
]
