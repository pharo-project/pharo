"
I am the delimiter for bold/strong format.
"
Class {
	#name : #MicBoldDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-ModelInline'
}

{ #category : #accessing }
MicBoldDelimiter class >> associatedInlineBlock [
	^ MicBoldInlineBlock 
]

{ #category : #accessing }
MicBoldDelimiter class >> isCloser [
	^ true
]

{ #category : #accessing }
MicBoldDelimiter class >> isOpener [
	^ true
]

{ #category : #accessing }
MicBoldDelimiter class >> markup [
	^ '**'
]

{ #category : #accessing }
MicBoldDelimiter class >> type [
	^ #bold
]
