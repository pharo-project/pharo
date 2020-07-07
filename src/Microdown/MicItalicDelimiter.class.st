"
I am the delimiter for italic/emphasis format.
"
Class {
	#name : #MicItalicDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-ModelInline'
}

{ #category : #accessing }
MicItalicDelimiter class >> associatedInlineBlock [
	^ MicItalicInlineBlock 
]

{ #category : #accessing }
MicItalicDelimiter class >> isCloser [
	^ true
]

{ #category : #accessing }
MicItalicDelimiter class >> isOpener [
	^ true
]

{ #category : #accessing }
MicItalicDelimiter class >> markup [
	^ '_'
]

{ #category : #accessing }
MicItalicDelimiter class >> type [
	^ #italic
]
