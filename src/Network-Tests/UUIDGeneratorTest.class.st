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
	previousUuid := generator next.
	"the internal counter inside generator will rollover at 16r10000,
	the least significant 10th byte rolls over every 16r100,
	the internal counter starts at a random value"
	2 * 16r10000 timesRepeat: [ 
		uuid := generator next.
		"same generator, consequetive counters (take roll-over into account)"
		self assert: ((previousUuid at: 10) + 1) \\ 256 equals: (uuid at: 10).
		previousUuid := uuid ]
]

{ #category : #testing }
UUIDGeneratorTest >> testDefault [
	self assert: UUIDGenerator default equals: UUIDGenerator default.
	self deny: UUIDGenerator next isNilUUID 
]

{ #category : #testing }
UUIDGeneratorTest >> testOne [
	| uuid |
	uuid := generator next.
	self assert: (uuid isKindOf: UUID).
	self deny: uuid isNilUUID
]

{ #category : #testing }
UUIDGeneratorTest >> testSpeed [
	self 
		should: [ 100000 timesRepeat: [ generator next ] ] 
		notTakeMoreThan: 1 second
]

{ #category : #testing }
UUIDGeneratorTest >> testTwoDifferentGenerator [
	| generator1 generator2 uuid1 uuid2 time1 time2 |
	generator1 := UUIDGenerator new.
	generator2 := UUIDGenerator new.
	uuid1 := generator1 next.
	uuid2 := generator2 next.
	"different UUIDs"
	self deny: uuid1 equals: uuid2.
	"different generators, different node id"
	self deny: uuid1 reverse readStream uint32 equals: uuid2 reverse readStream uint32.
	"same generators, similar clock, within 100 milliseconds (stored time is in microseconds)"
	time1 := self timeFromUUID: uuid1.
	time2 := self timeFromUUID: uuid2.
	self assert: (time2 - time1) abs < (100 * 1000)
]

{ #category : #testing }
UUIDGeneratorTest >> testTwoSameGenerator [
	| uuid1 uuid2 time1 time2 |
	uuid1 := generator next.
	uuid2 := generator next.
	"different UUIDs"
	self deny: uuid1 equals: uuid2.
	"same generator, same node id"
	self assert: uuid1 reverse readStream uint32 equals: uuid2 reverse readStream uint32.
	"same generator, consecutive counters (take roll-over into account)"
	self assert: ((uuid1 at: 10) + 1) \\ 256 equals: (uuid2 at: 10).
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
	count timesRepeat: [ uuids add: generator next ].
	self assert: uuids size equals: count
]

{ #category : #private }
UUIDGeneratorTest >> timeFromUUID: uuid [
	| bytes in |
	in := uuid readStream.
	bytes := ByteArray streamContents: [ :out |
		8 timesRepeat: [ out nextPut: in next ] ].
	^ bytes reverse asInteger 
]
