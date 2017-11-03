"
I implement how to write entries to a stream.
"
Class {
	#name : #OmEntryWriter,
	#superclass : #Object,
	#instVars : [
		'store'
	],
	#category : #Ombu-Persistence
}

{ #category : #'instance creation' }
OmEntryWriter class >> newForStore: aFileStore [
	
	^ self basicNew
		initializeWithStore: aFileStore;
		yourself

]

{ #category : #initialization }
OmEntryWriter >> initializeWithStore: aFileStore [

	self initialize.
	store := aFileStore.
]

{ #category : #writing }
OmEntryWriter >> on: aStream nextEntryPut: anEntry [ 
	"Write the entry on the stream."
	
	^ self subclassResponsibility
]

{ #category : #accessing }
OmEntryWriter >> store [
	^ store
]
