"
CRC failed
"
Class {
	#name : #CRCError,
	#superclass : #Error,
	#category : 'Compression-Streams'
}

{ #category : #testing }
CRCError >> isResumable [
	^true
]
