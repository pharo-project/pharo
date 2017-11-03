"
I represent a portion of a .ombu file. I'm a collaborator of OmBlockFileStore.
"
Class {
	#name : #OmBlock,
	#superclass : #Object,
	#instVars : [
		'firstEntryReference',
		'positions',
		'store',
		'startPosition',
		'endPosition'
	],
	#category : #Ombu-Stores
}

{ #category : #'instance creation' }
OmBlock class >> store: anOmBlockFileStore startPosition: aStartPosition endPosition: anEndPosition firstEntryReference: anOmReference [

	^ self basicNew
		initializeWithStore: anOmBlockFileStore
		startPosition: aStartPosition
		endPosition: anEndPosition
		firstEntryReference: anOmReference;
		yourself

]

{ #category : #accessing }
OmBlock >> atLocalName: aString ifPresent: presentBlockClosure ifAbsent: absentBlockClosure [

	| index |
	self checkIfMustRefreshBlock.

	index := aString asInteger - self firstLocalNameAsInteger + 1.
	
	^ index > positions size
		ifTrue: absentBlockClosure
		ifFalse: [
			presentBlockClosure value: (positions at: index) ]
]

{ #category : #refreshing }
OmBlock >> checkIfMustRefreshBlock [
	
	positions ifNil: [ self refresh ]
]

{ #category : #accessing }
OmBlock >> endPosition [
	^ endPosition
]

{ #category : #accessing }
OmBlock >> firstEntryReference [
	^ firstEntryReference
]

{ #category : #accessing }
OmBlock >> firstLocalNameAsInteger [
	^ self firstEntryReference localName asInteger
]

{ #category : #initialization }
OmBlock >> initializeWithStore: anOmBlockFileStore startPosition: aStartPosition endPosition: anEndPosition firstEntryReference: anOmReference [

	self initialize.
	store := anOmBlockFileStore.
	startPosition := aStartPosition.
	endPosition := anEndPosition.
	firstEntryReference := anOmReference.

]

{ #category : #accessing }
OmBlock >> lastLocalNameAsInteger [

	self checkIfMustRefreshBlock.
	
	^ positions size + self firstLocalNameAsInteger - 1.

]

{ #category : #accessing }
OmBlock >> positions [
	^ positions
]

{ #category : #printing }
OmBlock >> printOn: aStream [

	super printOn: aStream.
	
	aStream 
		nextPutAll: '(first entry: ';
		print: self firstLocalNameAsInteger;
		nextPutAll: ' positions: ';
		print: startPosition;
		nextPutAll: '..';
		print: endPosition;
		nextPut: $).

]

{ #category : #refreshing }
OmBlock >> refresh [
	"Read all OmEntry instances that belong to my range of file positions"
	
	positions := store entryPositionsStartingAt: startPosition upTo: endPosition
]

{ #category : #accessing }
OmBlock >> refreshIfNeededStartingAt: firstStreamPosition since: initialLocalName [ 

	positions isEmptyOrNil ifTrue: [ ^self ]. "Initial #refresh not yet done, so let things untouched."

	(self lastLocalNameAsInteger + 1) asString ~= initialLocalName ifTrue: [
		Halt now. "Shouldn't happen" ^self refresh ].

	"Then, it's only partially loaded i.e. outdated."
	positions addAll: (store entryPositionsStartingAt: firstStreamPosition upTo: endPosition).

]

{ #category : #accessing }
OmBlock >> startPosition [
	^ startPosition
]

{ #category : #accessing }
OmBlock >> store [
	^ store
]
