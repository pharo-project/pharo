"
I test the UUID Generator
"
Class {
	#name : #UUIDGeneratorTest,
	#superclass : #TestCase,
	#instVars : [
		'generator'
	],
	#category : #'Network-Tests-UUID'
}

{ #category : #running }
UUIDGeneratorTest >> setUp [
	super setUp.
	generator := UUIDGenerator new
]

{ #category : #testing }
UUIDGeneratorTest >> testCounterRollover [

	| previousUuid uuid |
	previousUuid := ByteArray new: 16.
	UUIDGenerator default placeFields: previousUuid.
	"the internal counter inside generator will rollover at 16r10000,
	the least significant 10th byte rolls over every 16r100,
	the internal counter starts at a random value"
	2 * 16r10000 timesRepeat: [
		uuid := ByteArray new: 16.
		UUIDGenerator default placeFields: uuid.
		"same generator, consequetive counters (take roll-over into account)"
		self assert: (previousUuid at: 10) + 1 \\ 256 equals: (uuid at: 10).
		previousUuid := uuid ]
]

{ #category : #testing }
UUIDGeneratorTest >> testDefault [

	self assert: UUIDGenerator default equals: UUIDGenerator default.
]

{ #category : #testing }
UUIDGeneratorTest >> testSpeed [
	|array|
	array := ByteArray new: 16.
	self
		should: [ 100000 timesRepeat: [ UUIDGenerator default placeFields: array ] ]
		notTakeMoreThan: 1 second
]

{ #category : #testing }
UUIDGeneratorTest >> testTwoDifferentGenerator [

	| generator1 generator2 uuid1 uuid2 time1 time2 |
	generator1 := UUIDGenerator new.
	generator2 := UUIDGenerator new.
	uuid1 := NilUUID new.
	uuid2 := NilUUID new.
	generator1 placeFields: uuid1 uuidData.
	generator2 placeFields: uuid2 uuidData.
	"different UUIDs"
	self deny: uuid1 equals: uuid2.
	"different generators, different node id"
	self
		deny: uuid1 uuidData reverse readStream uint32
		equals: uuid2 uuidData reverse readStream uint32.
	"same generators, similar clock, within 100 milliseconds (stored time is in microseconds)"
	time1 := self timeFromUUID: uuid1.
	time2 := self timeFromUUID: uuid2.
	"self assert: (time2 - time1) abs < (100 * 1000)"
	"UUID are not necessarly of version 1->3, time can be random"
]

{ #category : #testing }
UUIDGeneratorTest >> testTwoSameGenerator [

	| uuid1 uuid2 time1 time2 |
	uuid1 := NilUUID new.
	uuid2 := NilUUID new.
	UUIDGenerator default placeFields: uuid1 uuidData.
	UUIDGenerator default placeFields: uuid2 uuidData.
	"different UUIDs"
	self deny: uuid1 equals: uuid2.
	"same generator, same node id"
	self
		assert: uuid1 uuidData reverse readStream uint32
		equals: uuid2 uuidData reverse readStream uint32.
	"same generator, consecutive counters (take roll-over into account)"
	self assert: (uuid1 uuidData at: 10) + 1 \\ 256 equals: (uuid2 uuidData at: 10).
	"same generators, similar clock, within 100 milliseconds (stored time is in microseconds)"
	time1 := self timeFromUUID: uuid1.
	time2 := self timeFromUUID: uuid2.
	self assert: (time2 - time1) abs < (100 * 1000)
]

{ #category : #testing }
UUIDGeneratorTest >> testUniqueness [

	| uuids count |
	count := 100000.
	uuids := Set new: count.
	count timesRepeat: [ uuids add: UUID new ].
	self assert: uuids size equals: count
]

{ #category : #private }
UUIDGeneratorTest >> timeFromUUID: uuid [
	| bytes in |
	in := uuid uuidData readStream.
	bytes := ByteArray streamContents: [ :out |
		8 timesRepeat: [ out nextPut: in next ] ].
	^ bytes reverse asInteger
]
