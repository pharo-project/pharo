"
I am a log of system events.

A user of my instances is EpMonitor, who adds instances of EpEvent into me when certain system announcements happen.

My #entries contain the events ordered as ""the oldest first"".
 
Examples:

EpLog freshFromFile: 'path/to/ombu/file.ombu' asFileReference.
"
Class {
	#name : #EpLog,
	#superclass : #EpAbstractLog,
	#instVars : [
		'commentByEntryReference',
		'store'
	],
	#category : #'Epicea-Log'
}

{ #category : #'instance creation' }
EpLog class >> freshFromFile: aFileReference [

	^ (self fromFile: aFileReference)
		refresh;
		yourself
]

{ #category : #'instance creation' }
EpLog class >> fromFile: aFileReference [ 

	^ self newWithStore: (OmStoreFactory current fromFile: aFileReference)
]

{ #category : #'instance creation' }
EpLog class >> new [

	^ self newWithSessionStore
]

{ #category : #'instance creation' }
EpLog class >> newNull [

	^ self newWithStore: OmNullStore new.
]

{ #category : #'instance creation' }
EpLog class >> newWithEvents: eventsTheOldestFirst [
	^ self
		newWithStore:
			(OmMemoryStore withAll: eventsTheOldestFirst)
]

{ #category : #'instance creation' }
EpLog class >> newWithSessionStore [

	^ self newWithStore: OmSessionStore new.
]

{ #category : #'instance creation' }
EpLog class >> newWithStore: aStore [ 

	^ self basicNew
		initializeWith: aStore;
		yourself
]

{ #category : #accessing }
EpLog >> addEntryWith: anEvent tags: blockClosureForCustomTags [
	"Add an event with the specified tags"

	| newEntry |
	newEntry := OmEntry content: anEvent.

	"add tags"
	newEntry tags 
		at: self class priorReferenceKey put: self headReference;
		in: blockClosureForCustomTags.

	"write the new entry"
	store newEntry: newEntry.

	"update caches with the new entry"
	self cacheEntry: newEntry.

	self announceAdded: newEntry.

	^ newEntry
]

{ #category : #private }
EpLog >> cacheEntry: newEntry [
	"Update caches with a new entry"

	newEntry content isEpLogEntriesComment ifTrue: [ 
		(newEntry content entryReferences) do: [ :each |
			commentByEntryReference 
				at: each
				put: newEntry content comment 
		] ]
]

{ #category : #accessing }
EpLog >> commentAt: anEntry ifAbsent: aBlock [ 

	^ commentByEntryReference
		at: (self referenceTo: anEntry)
		ifAbsent: aBlock
]

{ #category : #accessing }
EpLog >> commentAt: anEntry ifPresent: aBlock [ 

	^ commentByEntryReference
		at: (self referenceTo: anEntry)
		ifPresent: aBlock
]

{ #category : #accessing }
EpLog >> entries [

	^ store entries
]

{ #category : #accessing }
EpLog >> entriesCount [
	
	^ store entriesCount
]

{ #category : #enumerating }
EpLog >> entriesDo: aBlockClosure [

	^ store entriesDo: aBlockClosure
]

{ #category : #accessing }
EpLog >> entryFor: aReference [

	^ store entryFor: aReference
]

{ #category : #accessing }
EpLog >> entryFor: aReference ifPresent: presentBlockClosure ifAbsent: absentBlockClosure [
	"Answer an entry, evaluating either the first block closure if present or the second if absent."
	
	^ store 
		entryFor: aReference
		ifPresent: presentBlockClosure
		ifAbsent: absentBlockClosure
]

{ #category : #accessing }
EpLog >> entryReferences [
	
	^ store entryReferences
]

{ #category : #accessing }
EpLog >> firstEntryIfAbsent: absentBlock [

	^ store firstEntryIfAbsent: absentBlock
]

{ #category : #accessing }
EpLog >> headReference [

	^ store headReference
]

{ #category : #initialization }
EpLog >> initializeWith: aStore [ 

	self initialize.
	store := aStore.
	commentByEntryReference := Dictionary new.
]

{ #category : #accessing }
EpLog >> referenceTo: anEntry [

	^ store referenceTo: anEntry
]

{ #category : #refreshing }
EpLog >> refresh [
	store refresh.
	commentByEntryReference := Dictionary new.
]

{ #category : #accessing }
EpLog >> store [

	^ store
]

{ #category : #private }
EpLog >> updateEntriesCache [

	store entriesDo: [ :each | self cacheEntry: each ]
]
