"
I provide a facade to create and reuse OmStore hierarchy instances.
"
Class {
	#name : #OmStoreFactory,
	#superclass : #Object,
	#instVars : [
		'nullStore',
		'storeByPath'
	],
	#classInstVars : [
		'current'
	],
	#category : #'Ombu-Stores'
}

{ #category : #accessing }
OmStoreFactory class >> current [

	^ current ifNil: [ current := self new ]
]

{ #category : #initialization }
OmStoreFactory class >> initialize [
	"
	self initialize
	"
	SessionManager default registerSystemClassNamed: self name
]

{ #category : #initialization }
OmStoreFactory class >> reset [
	"
	self reset
	"

	current := nil
]

{ #category : #'system startup' }
OmStoreFactory class >> shutDown: aboutToQuit [

	aboutToQuit ifTrue: [ self reset ]
]

{ #category : #'system startup' }
OmStoreFactory class >> startUp: isImageStarting [

	isImageStarting ifTrue: [ self reset ]
]

{ #category : #accessing }
OmStoreFactory >> defaultDirectory [

	^ OmSessionStore defaultBaseLocator asFileReference
]

{ #category : #accessing }
OmStoreFactory >> fromFile: aFileReference [

	^ self 
		named: (OmFileStore globalNameFrom: aFileReference)
		inDirectory: aFileReference parent
]

{ #category : #initialization }
OmStoreFactory >> initialize [
	super initialize.
	
	storeByPath := WeakValueDictionary new.

]

{ #category : #accessing }
OmStoreFactory >> named: aGlobalName [ 

	^ self named: aGlobalName inDirectory: self defaultDirectory
]

{ #category : #accessing }
OmStoreFactory >> named: aGlobalName inDirectory: aDirectory [

	| fileReference |
	fileReference := OmFileStore fileReferenceForStoreNamed: aGlobalName inDirectory: aDirectory.

	^ storeByPath
		at: fileReference fullName
		ifPresent: [:store | store ]
		ifAbsentOrNil: [ | newStore |
			newStore := self newStoreNamed: aGlobalName in: aDirectory.
			storeByPath at: fileReference fullName put: newStore.
			newStore ]

]

{ #category : #private }
OmStoreFactory >> newStoreNamed: aGlobalName in: aDirectory [

	^ OmBlockFileStore
		named: aGlobalName
		inDirectory: aDirectory
]

{ #category : #accessing }
OmStoreFactory >> null [

	^ nullStore ifNil: [ nullStore := OmNullStore new ]
]
