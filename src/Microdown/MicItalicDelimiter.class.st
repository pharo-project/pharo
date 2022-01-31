"
I am the delimiter for italic/emphasis format.
"
Class {
	#name : #MicItalicDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-Parser'
}

{ #category : #accessing }
MicItalicDelimiter class >> associatedInlineBlock [
	^ MicItalicFormatBlock 
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
