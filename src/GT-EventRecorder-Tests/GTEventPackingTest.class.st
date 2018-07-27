"
A GTEventPackingTest is a test class for testing the behavior of GTEventPacking
"
Class {
	#name : #GTEventPackingTest,
	#superclass : #TestCase,
	#instVars : [
		'packing',
		'event1',
		'event2',
		'collector',
		'bundle',
		'announcement',
		'unpackedData',
		'storage',
		'file'
	],
	#category : #'GT-EventRecorder-Tests-Support'
}

{ #category : #testing }
GTEventPackingTest class >> isAbstract [
	^ self name = GTEventPackingTest name
]

{ #category : #asserting }
GTEventPackingTest >> assertBundle [
	bundle := packing pack: collector.
	self assert: bundle url equals: collector url.
	self assert: bundle data isLiteral.
	self assert: bundle category equals: collector category.
]

{ #category : #asserting }
GTEventPackingTest >> assertUnpackedData [
	unpackedData := announcement unpackedData.
	self assert: unpackedData size equals: 2.
	self assert: unpackedData first equals: event1.
	self assert: unpackedData second equals: event2
]

{ #category : #accessing }
GTEventPackingTest >> packingClass [
	^ self subclassResponsibility
]

{ #category : #running }
GTEventPackingTest >> setUp [
	super setUp.
	event1 := GTDummyEvent new data: 'string'.
	event2 := GTDummyEvent new data: 42.
	collector := GTEventCollector new.
	collector
		add: event1;
		add: event2.
	collector category: #category1.
	packing := self packingClass new.
	storage := FileSystem memory.
	file := (storage workingDirectory / 'data')
		ensureCreateFile;
		yourself
]

{ #category : #running }
GTEventPackingTest >> tearDown [
	(storage workingDirectory / 'data') ensureDelete.
	super tearDown
]

{ #category : #'tests-packing' }
GTEventPackingTest >> testNewAnnouncementFor [
	announcement := packing newAnnouncementFor: collector.
	self assert: announcement serializedBy equals: packing class name.
	self assert: announcement eventRecorderVersion equals: packing version.
	self assert: announcement category equals: collector category.
]

{ #category : #'tests-packing' }
GTEventPackingTest >> testNewAnnouncementFor2 [
	collector updateAnnouncementBlock: [ :anAnnouncement | 
		anAnnouncement at: #testNewAnnouncementFor2 put: 42 ].
	announcement := packing newAnnouncementFor: collector.
	self assert: announcement serializedBy equals: packing class name.
	self assert: announcement eventRecorderVersion equals: packing version.
	self assert: (announcement at: #testNewAnnouncementFor2) equals: 42.
]

{ #category : #'tests-packing' }
GTEventPackingTest >> testPack [
	self assertBundle.
	announcement := packing unpack: bundle data.
	self assertUnpackedData
]

{ #category : #'tests-packing' }
GTEventPackingTest >> testUnpackBinaryBinaryStream [
	self assertBundle.
	file binaryWriteStreamDo: [ :aStream | 
		aStream nextPutAll: bundle data ].
	file binaryReadStreamDo: [ :aStream | 
		announcement := packing unpack: aStream contents ].
	self assertUnpackedData.

]

{ #category : #'tests-packing' }
GTEventPackingTest >> testUnpackBinaryUnspecifiedStream [
	self assertBundle.
	file binaryWriteStreamDo: [ :aStream | 
		aStream nextPutAll: bundle data ].
	announcement := packing unpack: (file binaryReadStreamDo: [ :stream | stream upToEnd ]).
	self assertUnpackedData.
]

{ #category : #'tests-packing' }
GTEventPackingTest >> testUnpackUnspecifiedBinaryStream [
	self assertBundle.
	file binaryWriteStreamDo: [ :aStream | 
		aStream nextPutAll: bundle data ].
	file binaryReadStreamDo: [ :aStream | 
		announcement := packing unpack: aStream contents ].
	self assertUnpackedData.

]

{ #category : #'tests-packing' }
GTEventPackingTest >> testUnpackUnspecifiedUnspecifiedStream [
	self assertBundle.
	file binaryWriteStreamDo: [ :aStream | 
		aStream nextPutAll: bundle data ].
	announcement := packing unpack: (file binaryReadStreamDo: [ :stream | stream upToEnd ]).
	self assertUnpackedData.
]
