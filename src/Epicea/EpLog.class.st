"
I am a log of system events (EpEvent), stored into entries (OmEntry).

A user of my instances is EpMonitor, who adds instances of EpEvent into me when certain system announcements happen.

My #entries contain the events ordered as ""the oldest first"".
 
Examples:

EpLog freshFromFile: 'path/to/ombu/file.ombu' asFileReference.
"
Class {
	#name : #EpLog,
	#superclass : #Object,
	#instVars : [
		'announcer',
		'commentByEntryReference',
		'store'
	],
	#category : #'Epicea-Log'
}

{ #category : #'tag keys' }
EpLog class >> authorKey [
	^ #author
]

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
EpLog class >> newWithEntries: entriesTheOldestFirst [
	^ self
		newWithStore: (OmMemoryStore withAllEntries: entriesTheOldestFirst)
]

{ #category : #'instance creation' }
EpLog class >> newWithEvents: eventsTheOldestFirst [
	^ self
		newWithStore:
			(OmMemoryStore withAllContents: eventsTheOldestFirst)
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

{ #category : #'tag keys' }
EpLog class >> priorReferenceKey [

	^ #prior
]

{ #category : #'tag keys' }
EpLog class >> timeKey [
	^ #time
]

{ #category : #'tag keys' }
EpLog class >> triggererReferenceKey [

	^ #trigger
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
EpLog >> announceAdded: anEntry [

	self announcer announce: (EpEntryAdded for: anEntry)
]

{ #category : #accessing }
EpLog >> announcer [

	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #accessing }
EpLog >> authorAt: anEntry [

	^ self 
		authorAt: anEntry 
		ifAbsent: [ self error ]
]

{ #category : #accessing }
EpLog >> authorAt: anEntry ifAbsent: aBlock [

	^ anEntry tags 
		at: self class authorKey
		ifAbsent: aBlock
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
	"Answer the String comment corresponding to anEntry, or evaluate aBlock if absent."

	^ commentByEntryReference
		at: (self referenceTo: anEntry)
		ifAbsent: aBlock
]

{ #category : #accessing }
EpLog >> commentAt: anEntry ifPresent: aBlock [ 
	"Answer the String comment corresponding to anEntry, and evaluate aBlock with it."

	^ commentByEntryReference
		at: (self referenceTo: anEntry)
		ifPresent: aBlock
]

{ #category : #accessing }
EpLog >> entries [
	"Answer the entries of this log."

	^ store entries
]

{ #category : #accessing }
EpLog >> entriesCount [
	
	^ store entriesCount
]

{ #category : #enumerating }
EpLog >> entriesDo: aBlockClosure [
	"Evaluate aBlockClosure on every entry"

	^ store entriesDo: aBlockClosure
]

{ #category : #accessing }
EpLog >> entriesForAll: references [

	^ references collect: [ :each | self entryFor: each ]
]

{ #category : #accessing }
EpLog >> entryFor: aReference [
	"Answer the entry corresponding to aReference"

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
EpLog >> events [

	^ self entries collect: #content
]

{ #category : #accessing }
EpLog >> firstEntryIfAbsent: absentBlock [
	"Answer the first entry of the log, or evaluate the absentBlock."

	^ store firstEntryIfAbsent: absentBlock
]

{ #category : #enumerating }
EpLog >> from: aReference detect: aBlockReturningBoolean [

	^ self
		from: aReference
		detect: aBlockReturningBoolean
		ifNotFound: [ :ref | KeyNotFound signalFor: aReference ]
]

{ #category : #enumerating }
EpLog >> from: aReference detect: aBlockReturningBoolean ifNotFound: notFoundBlock [

	self
		priorEntriesFrom: aReference
		do: [ :entry |
			(aBlockReturningBoolean value: entry)
				ifTrue: [ ^ entry ] ].
	^ notFoundBlock cull: aReference
]

{ #category : #enumerating }
EpLog >> fromHeadDetect: aBlockReturningBoolean [

	^ self
		from: self headReference
		detect: aBlockReturningBoolean
]

{ #category : #enumerating }
EpLog >> fromHeadDetect: aBlockReturningBoolean ifNotFound: notFoundBlock [

	^ self
		from: self headReference
		detect: aBlockReturningBoolean
		ifNotFound: notFoundBlock
]

{ #category : #testing }
EpLog >> hasAuthor: anEntry [

	self 
		authorAt: anEntry 
		ifAbsent: [ ^ false ].
		
	^ true
]

{ #category : #testing }
EpLog >> hasTime: anEntry [

	self 
		timeAt: anEntry 
		ifAbsent: [ ^ false ].
		
	^ true
]

{ #category : #accessing }
EpLog >> head [

	^ self entryFor: self headReference
]

{ #category : #accessing }
EpLog >> headReference [
	"Answer a OmReference to the head of this log"

	^ store headReference
]

{ #category : #initialization }
EpLog >> initializeWith: aStore [ 

	self initialize.
	store := aStore.
	commentByEntryReference := Dictionary new.
]

{ #category : #testing }
EpLog >> isEmpty [

	self entriesDo: [ :each | ^false ].
	^true
]

{ #category : #accessing }
EpLog >> nullReference [

	^ OmNullReference uniqueInstance
]

{ #category : #printing }
EpLog >> printOn: aStream [

	super printOn: aStream.
	aStream 
		nextPut: $(;
		print: self headReference;
		nextPut: $).
]

{ #category : #enumerating }
EpLog >> priorEntriesFrom: aReference [

	^ Array streamContents: [  :stream |
		self priorEntriesFrom: aReference do: [ :anEntry |
			stream nextPut: anEntry ] ]
]

{ #category : #enumerating }
EpLog >> priorEntriesFrom: aReference do: aBlock [

	| nextReference |
	nextReference := aReference.
	[ nextReference isNull ] whileFalse: [
		self 
			entryFor: nextReference 
			ifPresent: [ :entry | 
				aBlock value: entry.		
				nextReference := self priorReferenceAt: entry ]
			ifAbsent: [ ^ self ] ]

]

{ #category : #enumerating }
EpLog >> priorEntriesFrom: initialReference upTo: finalReference [

	| result |
	result := OrderedCollection new.
	
	self 
		priorEntriesFrom: initialReference 
		do: [ :anEntry |
			result add: anEntry.
			(self referenceTo: anEntry) = finalReference ifTrue: [ ^ result ].			
		].
	
	^ result
]

{ #category : #enumerating }
EpLog >> priorEntriesFromHead [

	^ self priorEntriesFrom: self headReference
]

{ #category : #enumerating }
EpLog >> priorEntriesFromHeadDo: aBlock [

	self priorEntriesFrom: self headReference do: aBlock
]

{ #category : #accessing }
EpLog >> priorReferenceAt: anEntry [

	^ anEntry tagAt: self class priorReferenceKey
]

{ #category : #accessing }
EpLog >> referenceTo: anEntry [

	^ store referenceTo: anEntry
]

{ #category : #accessing }
EpLog >> referencesToAll: aCollectionOfEntries [

	^ aCollectionOfEntries collect: [ :each | self referenceTo: each ]
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

{ #category : #accessing }
EpLog >> timeAt: anEntry [

	^ self 
		timeAt: anEntry 
		ifAbsent: [ self error ]
]

{ #category : #accessing }
EpLog >> timeAt: anEntry ifAbsent: aBlock [

	^ anEntry tags 
		at: self class timeKey
		ifAbsent: aBlock
]

{ #category : #accessing }
EpLog >> triggererReferenceOf: anEntry ifPresent: presentBlock ifAbsent: absentBlock [

	^ anEntry tags 
		at: self class triggererReferenceKey 
		ifPresent: presentBlock 
		ifAbsent: absentBlock
]

{ #category : #private }
EpLog >> updateEntriesCache [

	store entriesDo: [ :each | self cacheEntry: each ]
]
