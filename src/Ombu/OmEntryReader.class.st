"
I implement how to read entries from a stream.
"
Class {
	#name : #OmEntryReader,
	#superclass : #Object,
	#instVars : [
		'stream',
		'store'
	],
	#category : #Ombu-Persistence
}

{ #category : #'instance creation' }
OmEntryReader class >> newForStore: aFileStore [
	
	^ self basicNew
		initializeWithStore: aFileStore;
		yourself

]

{ #category : #reading }
OmEntryReader >> entryPositionsDo: aBlock [
	
	^ self subclassResponsibility
]

{ #category : #initialization }
OmEntryReader >> initializeWithStore: aFileStore [

	self initialize.
	store := aFileStore.
]

{ #category : #reading }
OmEntryReader >> nextEntry [
	"Read next entry from the stream."
	
	^ self subclassResponsibility
]

{ #category : #accessing }
OmEntryReader >> store [
	^ store
]

{ #category : #accessing }
OmEntryReader >> stream [

	^ stream
]

{ #category : #accessing }
OmEntryReader >> stream: aReadStream [

	stream := aReadStream
]
