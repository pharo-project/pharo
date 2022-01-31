"
An anchor reference represents a reference to a section, figure, environment. It follows the pattern `*@`, label, `*`

```
See *@fig1*
```
"
Class {
	#name : #MicAnnotationCloserDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-Parser'
}

{ #category : #accessing }
MicAnnotationCloserDelimiter class >> associatedInlineBlock [
	^ MicAnnotationBlock 
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

	^ '}?'
]

{ #category : #accessing }
MicAnnotationCloserDelimiter class >> type [
	^ #annotation
]
