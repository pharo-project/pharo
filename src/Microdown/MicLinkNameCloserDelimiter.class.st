"
I am the closer delimiter for link name.
"
Class {
	#name : #MicLinkNameCloserDelimiter,
	#superclass : #MicAbstractDelimiter,
	#category : #'Microdown-ModelInline'
}

{ #category : #accessing }
MicLinkNameCloserDelimiter class >> associatedInlineBlock [
	^ nil
]

{ #category : #accessing }
MicLinkNameCloserDelimiter class >> isCloser [
	^ true
]

{ #category : #accessing }
MicLinkNameCloserDelimiter class >> isOpener [
	^ false
]

{ #category : #accessing }
MicLinkNameCloserDelimiter class >> markup [
	^ ']'
]

{ #category : #accessing }
MicLinkNameCloserDelimiter class >> type [
	^ #linkName
]
