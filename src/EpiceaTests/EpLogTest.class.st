Class {
	#name : #EpLogTest,
	#superclass : #TestCase,
	#instVars : [
		'log',
		'logBuilder'
	],
	#category : #'EpiceaTests-Log'
}

{ #category : #running }
EpLogTest >> setUp [
	super setUp.
	
	logBuilder := EpTestLogBuilder new.
	log := logBuilder oneTriggerWithFourChildrenLog.
]

{ #category : #running }
EpLogTest >> tearDown [
		
	logBuilder cleanUp.
	super tearDown.
]

{ #category : #tests }
EpLogTest >> testEntries [
	self assert: log entries size equals: 5
]

{ #category : #tests }
EpLogTest >> testEntriesCount [
	self assert: log entriesCount equals: 5
]

{ #category : #tests }
EpLogTest >> testEntryReferences [

	| entryReferences entries |
	entryReferences := log entryReferences.
	entries := log entries.
	
	entryReferences withIndexDo: [:each :index |
		log
			entryFor: each
			ifPresent: [:entry | self assert: entry equals: (entries at: index) ]
			ifAbsent: [ self fail ] ].

	self assert: entryReferences size equals: 5.

]

{ #category : #tests }
EpLogTest >> testFirstEntryIfAbsent [
	self 
		assert: (log firstEntryIfAbsent: [ self fail ]) 
		equals: log entries first
]

{ #category : #tests }
EpLogTest >> testFromDetect [
	| reference isPrior entry |
	reference := log priorReferenceAt: log head.
	entry := log entryFor: reference.
	isPrior := false.
	" Assert for entry "
	self
		assert: (log from: reference detect: [ :dummy | true ])
		equals: entry.
	" Assert for the entry before 'entry' "
	self
		assert: (log
			from: reference
			detect: [ :dummy |
				isPrior
					ifFalse: [ isPrior := true. false ]
					ifTrue: [ true ] ])
		equals: (log entryFor: (log priorReferenceAt: entry)).
]

{ #category : #tests }
EpLogTest >> testFromDetectIfNotFound [
	| reference |
	reference := log priorReferenceAt: log head.
	" Assert for getting 42 when entry is not found. "
	self
		assert: (log
			from: reference
			detect: [ :dummy | false ]
			ifNotFound: [ 42 ])
		equals: 42.
]

{ #category : #tests }
EpLogTest >> testFromHeadDetect [

	" Assert for head "
	self
		assert: (log fromHeadDetect: [ :dummy | true ])
		equals: log head.
]

{ #category : #tests }
EpLogTest >> testFromHeadDetectIfNotFound [

	" Assert for getting 42 when entry is not found. "
	self
		assert: (log
			fromHeadDetect: [ :dummy | false ]
			ifNotFound: [ 42 ])
		equals: 42.
]

{ #category : #tests }
EpLogTest >> testFromNullReferenceDetectNotFound [

	self
		should: [ log from: log nullReference detect: [ :entry | true ] ]
		raise: KeyNotFound
]

{ #category : #tests }
EpLogTest >> testHeadReference [
	self deny: log headReference isNull
]

{ #category : #tests }
EpLogTest >> testPriorEntriesFromNullReferenceIsEmpty [
	| priorEntries |
	priorEntries := log priorEntriesFrom: log nullReference.
	self assert: priorEntries isEmpty
]

{ #category : #tests }
EpLogTest >> testShouldKeepHeadReferenceAfterReOpen [
	| anotherLog |

	anotherLog := EpLog newWithStore: log store copy refresh.

	self assert: anotherLog headReference equals: log headReference.
]

{ #category : #tests }
EpLogTest >> testShouldKeepHeadReferenceAfterRefresh [
	| headReference |
	headReference := log headReference.

	log refresh.

	self assert: log headReference equals: headReference.
]
