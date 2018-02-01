"
I am an abstract test case for Ombu stores. My subclasses implement a hook to answer new concrete stores.
"
Class {
	#name : #OmStoreTest,
	#superclass : #TestCase,
	#instVars : [
		'store',
		'anEntry',
		'aReference',
		'otherStore'
	],
	#category : #OmbuTests
}

{ #category : #testing }
OmStoreTest class >> isAbstract [
	^ self name = #OmStoreTest
]

{ #category : #running }
OmStoreTest >> beforeAndAfterFlushStore: aBlock [ 

	aBlock value.
	store flush. "Reason: It's worth testing again, after flush."
	aBlock value.
]

{ #category : #running }
OmStoreTest >> setUp [
	super setUp.
	
	store := self setUpStore.
	otherStore := self setUpOtherStore.
]

{ #category : #resources }
OmStoreTest >> setUpOtherStore [
	^ self setUpStore
]

{ #category : #resources }
OmStoreTest >> setUpStore [
	^ self subclassResponsibility
]

{ #category : #running }
OmStoreTest >> tearDown [
	
	store flush.
	otherStore flush.
	super tearDown

]

{ #category : #tests }
OmStoreTest >> testEntries [
	self assert: store entries isEmpty.
]

{ #category : #tests }
OmStoreTest >> testEntriesCount [
	self assert: store entriesCount equals: 0.
	
	store newEntry: (OmEntry content: 42).
	store newEntry: (OmEntry content: 43).
	
	self beforeAndAfterFlushStore: [
		self assert: store entriesCount equals: 2 ].
]

{ #category : #tests }
OmStoreTest >> testEntriesDo [
	1 to: 7 do: [ :each | store newEntry: (OmEntry content: each) ].

	self beforeAndAfterFlushStore: [
		| contents |
		contents := Array streamContents: [ :stream | 
			store entriesDo: [ :entry | stream nextPut: entry content ] ].

		self assert: contents equals: (1 to: 7) asArray ]
]

{ #category : #tests }
OmStoreTest >> testEntryForAbsentEntry [
	| isPresent isAbsent |
	anEntry := OmEntry content: 42.
	
	store newEntry: anEntry.

	self beforeAndAfterFlushStore: [
		isPresent := false.
		isAbsent := false.	

		otherStore 
			entryFor: (store referenceTo: anEntry)
			ifPresent: [ :e | isPresent := true ] 
			ifAbsent: [ isAbsent := true ].

		self deny: isPresent.
		self assert: isAbsent ]
]

{ #category : #tests }
OmStoreTest >> testEntryForNullReference [
	| isPresent isAbsent |

	self beforeAndAfterFlushStore: [
		isPresent := false.
		isAbsent := false.	

		otherStore 
			entryFor: OmNullReference uniqueInstance
			ifPresent: [ :e | isPresent := true ] 
			ifAbsent: [ isAbsent := true ].

		self deny: isPresent.
		self assert: isAbsent ]
]

{ #category : #tests }
OmStoreTest >> testEntryForPresentEntry [
	| isPresent isAbsent |
	anEntry := OmEntry content: 42.
	
	store newEntry: anEntry.

	self beforeAndAfterFlushStore: [
		isPresent := false.
		isAbsent := false.	

		store 
			entryFor: (store referenceTo: anEntry)
			ifPresent: [ :e | isPresent := true ] 
			ifAbsent: [ isAbsent := true ].

		self assert: isPresent.
		self deny: isAbsent ]
]

{ #category : #tests }
OmStoreTest >> testEntryReferences [
	| entryReferences entries |

	1 to: 7 do: [ :each | store newEntry: (OmEntry content: each) ].

	entryReferences := store entryReferences.
	entries := store entries.
	
	entryReferences withIndexDo: [:each :index |	
		store
			entryFor: each
			ifPresent: [:entry | 
				self assert: entry equals: (entries at: index) 
				"They must have the same order" ]
			ifAbsent: [ self fail ] ].

	self assert: entryReferences size equals: 7. "To be sure it tests something"

]

{ #category : #tests }
OmStoreTest >> testFirstEntryIfAbsent [

	| absent |
	absent := false.
	store firstEntryIfAbsent: [ absent := true ].
	self assert: absent.
	
	store newEntry: (OmEntry content: 42).
	store newEntry: (OmEntry content: 43).
	
	self beforeAndAfterFlushStore: [
		self 
			assert: (store firstEntryIfAbsent: [ self fail ])
			equals: store entries first ]
]

{ #category : #tests }
OmStoreTest >> testHeadReference [
	1 to: 7 do: [ :each | store newEntry: (OmEntry content: each) ].

	self beforeAndAfterFlushStore: [
		| lastEntry |
		lastEntry := store entries detect: [ :entry | entry content = 7 ].
		self
			assert: store headReference
			equals: (store referenceTo: lastEntry) ]
]

{ #category : #tests }
OmStoreTest >> testHeadReferenceForEmptyStore [

	self assert: store headReference equals: OmNullReference uniqueInstance.
]

{ #category : #tests }
OmStoreTest >> testMultipleEntries [
	1 to: 7 do: [ :each | store newEntry: (OmEntry content: each) ].
	
	self beforeAndAfterFlushStore: [
		self
			assert: (store entries collect: #content as: Set)
			equals: (1 to: 7) asSet ]
]

{ #category : #tests }
OmStoreTest >> testNewEntry [
	anEntry := OmEntry content: 42.
	
	store newEntry: anEntry.
	
	self beforeAndAfterFlushStore: [
		self 
			assert: (store entries collect: #content as: Array)
			equals: #(42) ]
]

{ #category : #tests }
OmStoreTest >> testReferenceTo [
	anEntry := OmEntry content: 42.
	
	store newEntry: anEntry.
	
	self beforeAndAfterFlushStore: [
		aReference := store referenceTo: anEntry.

		self assert: (aReference isKindOf: OmReference).
		self deny: aReference isNull ]
]

{ #category : #tests }
OmStoreTest >> testRefreshEmpty [
	store refresh.
	
	self assert: store entries isEmpty.
]

{ #category : #tests }
OmStoreTest >> testRefreshNotEmpty [
	anEntry := OmEntry content: 42.
	
	store newEntry: anEntry.
	
	store refresh.
	
	self 
		assert: (store entries collect: #content as: Array)
		equals: #(42)
]

{ #category : #tests }
OmStoreTest >> testWritingFileReference [

	anEntry := OmEntry content: 42.
	store newEntry: anEntry.
	store flush.
	
	self assert: store writingFileReference isFile.
]
