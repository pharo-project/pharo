"
I am a notification raised when a OmFileStore has a problem reading an entry from its file.
"
Class {
	#name : #OmFileStoreReadingError,
	#superclass : #Error,
	#category : #Ombu-Errors
}

{ #category : #'instance creation' }
OmFileStoreReadingError class >> readingError: sourceError on: aFileReference position: aFilePosition [

	^ self new messageText: 'Reading entry from ', aFileReference asString, '@', aFilePosition asString, ': ', sourceError messageText 
]
