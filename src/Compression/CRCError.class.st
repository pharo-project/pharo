"
CRC failed
"
Class {
	#name : 'CRCError',
	#superclass : 'Error',
	#category : 'Compression-Streams',
	#package : 'Compression',
	#tag : 'Streams'
}

{ #category : 'testing' }
CRCError >> isResumable [

	^ true
]
