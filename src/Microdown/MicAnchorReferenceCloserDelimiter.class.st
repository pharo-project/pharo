"
An anchor reference represents a reference to a section, figure, environment. It follows the pattern `*@`, label, `@*`

```
See *@fig1@*
```
"
Class {
	#name : #MicAnchorReferenceCloserDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-Parser'
}

{ #category : #accessing }
MicAnchorReferenceCloserDelimiter class >> associatedInlineBlock [
	^ MicAnchorReferenceBlock 
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
