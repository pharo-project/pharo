"
A GTEventCollectorsTest is a test class for testing the behavior of GTEventCollectors
"
Class {
	#name : #GTEventCollectorsTest,
	#superclass : #TestCase,
	#instVars : [
		'collectors'
	],
	#category : #'GT-EventRecorder-Tests-Support'
}

{ #category : #running }
GTEventCollectorsTest >> setUp [
	super setUp.
	collectors := GTEventCollectors new.
]

{ #category : #tests }
GTEventCollectorsTest >> testAdd [
	collectors add: GTEventCollector new.
	self assert: collectors size equals: 1.
]

{ #category : #tests }
GTEventCollectorsTest >> testAdd2 [
	collectors add: GTEventCollector new.
	collectors add: GTEventCollector new.
	self assert: collectors size equals: 2.
]

{ #category : #tests }
GTEventCollectorsTest >> testAdd3 [
	| collector |
	collector := GTEventCollector new.
	collectors add: collector.
	collectors add: collector.
	self assert: collectors size equals: 1.
]

{ #category : #tests }
GTEventCollectorsTest >> testDefault [
	self assert: collectors size equals: 0.
]

{ #category : #tests }
GTEventCollectorsTest >> testDo [
	"Ensure that collectors are iterated in the same order as they are added."
	| collector1 collector2 interatedCollectors |
	collector1 := GTEventCollector new.
	collector2 := GTEventCollector new.
	collectors add: collector2.
	collectors add: collector1.
	interatedCollectors := OrderedCollection new.
	collectors do: [ :eachCollector | interatedCollectors addLast: eachCollector ].
	self assert: interatedCollectors size equals: 2.
	self assert: interatedCollectors first == collector2.
	self assert: interatedCollectors second == collector1.
	
]

{ #category : #tests }
GTEventCollectorsTest >> testDo2 [
	"Ensure that collectors are iterated in the same order as they are added."
	| collector1 collector2 interatedCollectors |
	collector1 := GTEventCollector new.
	collector2 := GTEventCollector new.
	collectors add: collector1.
	collectors add: collector2.
	interatedCollectors := OrderedCollection new.
	collectors do: [ :eachCollector | interatedCollectors addLast: eachCollector ].
	self assert: interatedCollectors size equals: 2.
	self assert: interatedCollectors first == collector1.
	self assert: interatedCollectors second == collector2.
	
]

{ #category : #tests }
GTEventCollectorsTest >> testPrintOn [
	self assert: (String streamContents: [ :stream | collectors printOn: stream ]) notEmpty.
]
