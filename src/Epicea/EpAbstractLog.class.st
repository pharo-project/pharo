"
I am an abstract log of system events (EpEvent), stored into entries (OmEntry).
"
Class {
	#name : #EpAbstractLog,
	#superclass : #Object,
	#instVars : [
		'announcer'
	],
	#category : #Epicea-Log
}

{ #category : #'tag keys' }
EpAbstractLog class >> authorKey [
	^ #author
]

{ #category : #'tag keys' }
EpAbstractLog class >> priorReferenceKey [

	^ #prior
]

{ #category : #'tag keys' }
EpAbstractLog class >> timeKey [
	^ #time
]

{ #category : #'tag keys' }
EpAbstractLog class >> triggererReferenceKey [

	^ #trigger
]

{ #category : #private }
EpAbstractLog >> announceAdded: anEntry [

	self announcer announce: (EpEntryAdded for: anEntry)
]

{ #category : #accessing }
EpAbstractLog >> announcer [

	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #accessing }
EpAbstractLog >> authorAt: anEntry [

	^ self 
		authorAt: anEntry 
		ifAbsent: [ self error ]
]

{ #category : #accessing }
EpAbstractLog >> authorAt: anEntry ifAbsent: aBlock [

	^ anEntry tags 
		at: self class authorKey
		ifAbsent: aBlock
]

{ #category : #accessing }
EpAbstractLog >> commentAt: anEntry ifAbsent: aBlock [ 
	"Answer the String comment corresponding to anEntry, or evaluate aBlock if absent."

	^ self subclassResponsibility
]

{ #category : #accessing }
EpAbstractLog >> commentAt: anEntry ifPresent: aBlock [ 
	"Answer the String comment corresponding to anEntry, and evaluate aBlock with it."

	| comment |
	comment := self commentAt: anEntry ifAbsent: [ ^self ].
	^ aBlock value: comment.
]

{ #category : #accessing }
EpAbstractLog >> entries [
	"Answer the entries of this log."

	^ self subclassResponsibility
]

{ #category : #accessing }
EpAbstractLog >> entriesCount [
	
	^ self entries size
]

{ #category : #enumerating }
EpAbstractLog >> entriesDo: aBlockClosure [
	"Evaluate aBlockClosure on every entry"
	
	^ self entries do: aBlockClosure
]

{ #category : #accessing }
EpAbstractLog >> entriesForAll: references [

	^ references collect: [ :each | self entryFor: each ]
]

{ #category : #accessing }
EpAbstractLog >> entryFor: aReference [
	"Answer the entry corresponding to aReference"

	^ self subclassResponsibility
]

{ #category : #accessing }
EpAbstractLog >> entryFor: aReference ifPresent: presentBlockClosure ifAbsent: absentBlockClosure [
	"Answer an entry, evaluating either the first block closure if present or the second if absent."
	
	^ self subclassResponsibility
]

{ #category : #accessing }
EpAbstractLog >> entryReferences [
	
	^ self referencesToAll: self entries
]

{ #category : #accessing }
EpAbstractLog >> events [

	^ self entries collect: #content
]

{ #category : #accessing }
EpAbstractLog >> firstEntryIfAbsent: absentBlock [
	"Answer the first entry of the log, or evaluate the absentBlock."

	^ self entries ifEmpty: absentBlock ifNotEmpty: [:collection | collection first ]
]

{ #category : #enumerating }
EpAbstractLog >> from: aReference detect: aBlockReturningBoolean [

	^ self
		from: aReference
		detect: aBlockReturningBoolean
		ifNotFound: [ :ref | KeyNotFound signalFor: aReference ]
]

{ #category : #enumerating }
EpAbstractLog >> from: aReference detect: aBlockReturningBoolean ifNotFound: notFoundBlock [

	self
		priorEntriesFrom: aReference
		do: [ :entry |
			(aBlockReturningBoolean value: entry)
				ifTrue: [ ^ entry ] ].
	^ notFoundBlock cull: aReference
]

{ #category : #enumerating }
EpAbstractLog >> fromHeadDetect: aBlockReturningBoolean [

	^ self
		from: self headReference
		detect: aBlockReturningBoolean
]

{ #category : #enumerating }
EpAbstractLog >> fromHeadDetect: aBlockReturningBoolean ifNotFound: notFoundBlock [

	^ self
		from: self headReference
		detect: aBlockReturningBoolean
		ifNotFound: notFoundBlock
]

{ #category : #testing }
EpAbstractLog >> hasAuthor: anEntry [

	self 
		authorAt: anEntry 
		ifAbsent: [ ^ false ].
		
	^ true
]

{ #category : #testing }
EpAbstractLog >> hasTime: anEntry [

	self 
		timeAt: anEntry 
		ifAbsent: [ ^ false ].
		
	^ true
]

{ #category : #accessing }
EpAbstractLog >> head [

	^ self entryFor: self headReference
]

{ #category : #accessing }
EpAbstractLog >> headReference [
	"Answer a OmReference to the head of this log"

	^ self subclassResponsibility
]

{ #category : #testing }
EpAbstractLog >> isEmpty [

	self entriesDo: [ :each | ^false ].
	^true
]

{ #category : #accessing }
EpAbstractLog >> nullReference [

	^ OmNullReference uniqueInstance
]

{ #category : #printing }
EpAbstractLog >> printOn: aStream [

	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		nextPutAll: self headReference asString;
		nextPut: $).
]

{ #category : #enumerating }
EpAbstractLog >> priorEntriesFrom: aReference [

	^ Array streamContents: [  :stream |
		self priorEntriesFrom: aReference do: [ :anEntry |
			stream nextPut: anEntry ] ]
]

{ #category : #enumerating }
EpAbstractLog >> priorEntriesFrom: aReference do: aBlock [

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
EpAbstractLog >> priorEntriesFrom: initialReference upTo: finalReference [

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
EpAbstractLog >> priorEntriesFromHead [

	^ self priorEntriesFrom: self headReference
]

{ #category : #enumerating }
EpAbstractLog >> priorEntriesFromHeadDo: aBlock [

	self priorEntriesFrom: self headReference do: aBlock
]

{ #category : #accessing }
EpAbstractLog >> priorReferenceAt: anEntry [

	^ anEntry tagAt: self class priorReferenceKey
]

{ #category : #accessing }
EpAbstractLog >> referenceTo: anEntry [
	"Answer a OmReference corresponding to anEntry."

	^ self subclassResponsibility
]

{ #category : #accessing }
EpAbstractLog >> referencesToAll: aCollectionOfEntries [

	^ aCollectionOfEntries collect: [ :each | self referenceTo: each ]
]

{ #category : #refreshing }
EpAbstractLog >> refresh [
	"Do nothing by default"
]

{ #category : #accessing }
EpAbstractLog >> timeAt: anEntry [

	^ self 
		timeAt: anEntry 
		ifAbsent: [ self error ]
]

{ #category : #accessing }
EpAbstractLog >> timeAt: anEntry ifAbsent: aBlock [

	^ anEntry tags 
		at: self class timeKey
		ifAbsent: aBlock
]

{ #category : #accessing }
EpAbstractLog >> triggererReferenceOf: anEntry ifPresent: presentBlock ifAbsent: absentBlock [

	^ anEntry tags 
		at: self class triggererReferenceKey 
		ifPresent: presentBlock 
		ifAbsent: absentBlock
]
