"
I am an in-memory store of entries.
"
Class {
	#name : #OmMemoryStore,
	#superclass : #OmStore,
	#instVars : [
		'entries',
		'globalName'
	],
	#category : #'Ombu-Stores'
}

{ #category : #'instance creation' }
OmMemoryStore class >> named: aGlobalName [

	^ self basicNew
		initializeWithGlobalName: aGlobalName;
		yourself
]

{ #category : #'instance creation' }
OmMemoryStore class >> new [

	^ self named: UUID new asString36
]

{ #category : #'instance creation' }
OmMemoryStore class >> withAllContents: aCollection [
	| entries |
	entries := aCollection collect: [ :each | OmEntry content: each ].
	^ self withAllEntries: entries
]

{ #category : #'instance creation' }
OmMemoryStore class >> withAllEntries: aCollection [
	^ self new
		in: [ :store | aCollection do: [ :entry | store newEntry: entry ] ];
		yourself
]

{ #category : #accessing }
OmMemoryStore >> entries [
	^ entries
]

{ #category : #enumerating }
OmMemoryStore >> entriesDo: aBlockClosure [ 
	entries do: aBlockClosure
]

{ #category : #accessing }
OmMemoryStore >> entryFor: aReference ifPresent: presentBlockClosure ifAbsent: absentBlockClosure [

	| entry |
	(aReference isNull or: [ aReference globalName ~= self globalName])
		ifTrue: [ ^ absentBlockClosure value ].

	entry := entries
		at: aReference localName asInteger
		ifAbsent: [ ^ absentBlockClosure value ].
		
	^ presentBlockClosure cull: entry
]

{ #category : #accessing }
OmMemoryStore >> globalName [
	^ globalName
]

{ #category : #accessing }
OmMemoryStore >> headReference [
	"Answer a reference to the last entry added to this store, or OmNullReference uniqueInstance when empty."

	^ entries
		ifEmpty: [ OmNullReference uniqueInstance ]
		ifNotEmpty: [ self referenceTo: entries last ]
]

{ #category : #accessing }
OmMemoryStore >> initializeWithGlobalName: aName [

	self initialize.
	globalName := aName.
	
	entries := OrderedCollection new.
]

{ #category : #writing }
OmMemoryStore >> newEntry: anEntry [

	| localName newReference |
	localName := (entries size + 1) asString.
	newReference := self referenceToLocalName: localName.
	anEntry tags at: self selfReferenceKey put: newReference.
	entries add: anEntry.

]

{ #category : #refreshing }
OmMemoryStore >> refresh [
	"Nothing to refresh"
	
	^ self
]

{ #category : #accessing }
OmMemoryStore >> writingFileReference [
	"Stub"

	^ (FileSystem memory / 'store.ombu') ensureCreateFile
]
