"
I am the opener delimiter for figure name.
"
Class {
	#name : #MicFigureNameOpenerDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-Parser'
}

{ #category : #accessing }
MicFigureNameOpenerDelimiter class >> associatedInlineBlock [
	^ MicFigureBlock
]

{ #category : #accessing }
MicFigureNameOpenerDelimiter class >> isCloser [
	^ false
]

{ #category : #accessing }
MicFigureNameOpenerDelimiter class >> isOpener [
	^ true
]

{ #category : #accessing }
MicFigureNameOpenerDelimiter class >> markup [
	^ '!['
]

{ #category : #accessing }
MicFigureNameOpenerDelimiter class >> type [
	^ #figure
]
