"
I am a single-file store of entries. Each new entry is appended at the end. Entries are lazily read in blocks (represented by OmBlock) from file on demand.
"
Class {
	#name : #OmBlockFileStore,
	#superclass : #OmFileStore,
	#instVars : [
		'blocks'
	],
	#category : #'Ombu-Stores'
}

{ #category : #private }
OmBlockFileStore >> blockForLocalNameAsInteger: anIndex [
	
	blocks reverseDo: [:each | each firstLocalNameAsInteger <= anIndex ifTrue: [ ^each ] ].
	^ self error: 'block not found'
]

{ #category : #accessing }
OmBlockFileStore >> blockSize [

	^  131072 "2 ** 17"
]

{ #category : #accessing }
OmBlockFileStore >> entryPositionsStartingAt: startPosition upTo: endPosition [

	self readEntriesWith: [:readStream |
		readStream position: startPosition.
		^self newEntryReader
			stream: readStream;
			entryPositionsUpTo: endPosition ]
]

{ #category : #initialization }
OmBlockFileStore >> initialize [

	super initialize.
	
	blocks := OrderedCollection with:
		(OmBlock
			store: self
			startPosition: 0
			endPosition: self blockSize - 1
			firstEntryReference: (self referenceToLocalName: '1')).
]

{ #category : #accessing }
OmBlockFileStore >> readEntryForLocalName: aString ifPresent: presentBlockClosure ifAbsent: absentBlockClosure [

	^ (self blockForLocalNameAsInteger: aString asInteger)
			atLocalName: aString
			ifPresent: [ :position |
				presentBlockClosure value: (self nextEntryFromPosition: position) ]
			ifAbsent: absentBlockClosure

]

{ #category : #refreshing }
OmBlockFileStore >> refresh [

	self flush.

	self critical: [
		mustRefresh := false.
		self initialize.
		self fileReference exists ifFalse: [ ^self ].
		self readEntriesWith: [:readStream |
			self refreshNewBlocksFrom: readStream.
			self refreshHeadReferenceAndEntryCountFrom: readStream. ]
		]
	
]

{ #category : #private }
OmBlockFileStore >> refreshEntryPositionsByLocalNameStartingAt: firstStreamPosition since: initialLocalName [
	"AKA: Refresh blocks after new entries were written."

	| lastBlock |
	lastBlock := blocks last.

	"Update last block if necessary"
	firstStreamPosition < lastBlock endPosition ifTrue: [ 
		lastBlock refreshIfNeededStartingAt: firstStreamPosition since: initialLocalName ].

	"Add new blocks if necessary"
	lastStreamPosition > lastBlock endPosition ifTrue: [
		self readEntriesWith: [:readStream | self refreshNewBlocksFrom: readStream ] ].

]

{ #category : #refreshing }
OmBlockFileStore >> refreshHeadReferenceAndEntryCountFrom: readStream [

	entryCount := blocks last lastLocalNameAsInteger.
	headReference := self referenceToLocalName: entryCount asString.

]

{ #category : #refreshing }
OmBlockFileStore >> refreshNewBlocksFrom: readStream [

	| reader blockPosition blockFirstEntry |
	reader := self newEntryReader.
	reader stream: readStream.
	
	[	blockPosition := blocks size * self blockSize.
		blockPosition < readStream size ] whileTrue: [
			readStream position: blockPosition.
			reader nextEntryPositionIfFound: [:position | 
				readStream position: position.
				blockFirstEntry := reader nextEntry.
				blocks add: (OmBlock
					store: self
					startPosition: position
					endPosition: blockPosition + self blockSize - 1
					firstEntryReference: (self referenceTo: blockFirstEntry)).
				] ifNone: [ ^self "No more entries to add in a new block" ].
		]

]
