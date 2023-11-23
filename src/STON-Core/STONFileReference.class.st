"
I am STONFileReference, I am an implementation artifact to help reading objects with class tag equal to my #stonName.

I am a FileReference.
"
Class {
	#name : 'STONFileReference',
	#superclass : 'FileReference',
	#category : 'STON-Core-Reader',
	#package : 'STON-Core',
	#tag : 'Reader'
}

{ #category : 'ston' }
STONFileReference class >> fromSton: stonReader [
	^ stonReader parseListSingleton asFileReference
]

{ #category : 'ston' }
STONFileReference class >> stonName [
	^ #FILE
]
