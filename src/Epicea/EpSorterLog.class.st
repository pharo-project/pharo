"
I'm a log whose entries can have an arbitrary order and origin.
"
Class {
	#name : #EpSorterLog,
	#superclass : #EpAbstractLog,
	#instVars : [
		'entries'
	],
	#category : #'Epicea-Log'
}

{ #category : #accessing }
EpSorterLog >> commentAt: anEntry ifAbsent: aBlock [
	"Stub"

	^ aBlock value
]

{ #category : #accessing }
EpSorterLog >> entries [
	^ entries
]

{ #category : #accessing }
EpSorterLog >> entries: anObject [
	entries := anObject
]

{ #category : #accessing }
EpSorterLog >> entryFor: aReference [
	"Answer the entry corresponding to aReference"

	^ self
		entryFor: aReference
		ifPresent: [ :anEntry | ^ anEntry ] 
		ifAbsent: [ NotFound signalFor: aReference ]

]

{ #category : #accessing }
EpSorterLog >> entryFor: aReference ifPresent: presentBlockClosure ifAbsent: absentBlockClosure [
	"Answer an entry, evaluating either the first block closure if present or the second if absent."

	self entriesDo: [:each | 
		(self referenceTo: each) = aReference 
			ifTrue: [ ^ presentBlockClosure cull: each ] ].
	
	^ absentBlockClosure value
]

{ #category : #accessing }
EpSorterLog >> headReference [
	"Answer a OmReference to the head of this log"

	self flag: #todo. "Maybe this is a concept that only belongs to EpLog... remove from superclass?"
	self shouldBeImplemented
]

{ #category : #accessing }
EpSorterLog >> referenceTo: anEntry [
	"Answer a OmReference corresponding to anEntry."

	^ anEntry tags at: OmStore selfReferenceKey
]
