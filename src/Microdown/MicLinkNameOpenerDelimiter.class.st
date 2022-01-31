"
I am the opener delimiter for link name.
"
Class {
	#name : #MicLinkNameOpenerDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-Parser'
}

{ #category : #accessing }
MicLinkNameOpenerDelimiter class >> associatedInlineBlock [
	^ MicLinkBlock 
]

{ #category : #accessing }
MicLinkNameOpenerDelimiter class >> isCloser [
	^ false
]

{ #category : #accessing }
MicLinkNameOpenerDelimiter class >> isOpener [
	^ true
]

{ #category : #accessing }
MicLinkNameOpenerDelimiter class >> markup [
	^ '['
]

{ #category : #accessing }
MicLinkNameOpenerDelimiter class >> type [
	^ #linkName
]
