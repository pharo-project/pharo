"
This is the unit test for the class Morph. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- http://minnow.cc.gatech.edu/squeak/1547
	- the sunit class category
"
Class {
	#name : #MorphTest,
	#superclass : #TestCase,
	#instVars : [
		'morph',
		'world'
	],
	#category : #'Morphic-Tests-Kernel'
}

{ #category : #running }
MorphTest >> setUp [
	super setUp.
	morph := Morph new
]

{ #category : #running }
MorphTest >> tearDown [
	morph delete.
	super tearDown
]

{ #category : #'testing - geometry' }
MorphTest >> testBoundingBoxOfSubmorphs [

	| subMorph |
	subMorph := morph copy
		topLeft: morph topLeft + 5;
		yourself.

	morph addMorph: subMorph.
	
	self assert: morph boundingBoxOfSubmorphs equals: subMorph bounds.
		
]

{ #category : #'testing - geometry' }
MorphTest >> testExtent [
	| m1 m2 v1 v2 v3 b1 b2 |
	m1 := Morph new.
	m2 := Morph new.
	
	v1 := 100.000001.
	v2 := 100.000001000001.
	v3 := 100.000001000002.

	m1 extent: v1@v1.
	b1 := m1 bounds.
	
	m2 extent: v2@v3.
	b2 := m2 bounds.
	
	self assert: (b2 = b1).

]

{ #category : #'as yet unclassified' }
MorphTest >> testHaloIsDisable [
	| isHaloEnable |
	isHaloEnable := Morph halosEnabled.
	Morph halosEnabled: false.
	self assert: (Morph new addHalo: nil) ~= (Smalltalk globals at: #HaloMorph).
	Morph halosEnabled: isHaloEnable
]

{ #category : #'testing - into/outof world' }
MorphTest >> testIntoWorldCollapseOutOfWorld [
	| m1 m2 collapsed |
	"Create the guys"
	m1 := TestInWorldMorph new.
	m2 := TestInWorldMorph new.
	self assert: (m1 intoWorldCount = 0).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 0).
	self assert: (m2 outOfWorldCount = 0).

	"add them to basic morph"
	morph addMorphFront: m1.
	m1 addMorphFront: m2.
	self assert: (m1 intoWorldCount = 0).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 0).
	self assert: (m2 outOfWorldCount = 0).

	"open the guy"
	morph openInWorld.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 0).

	"collapse it"
	collapsed := 	CollapsedMorph new beReplacementFor: morph.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 1).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 1).

	"expand it"
	collapsed collapseOrExpand.
	self assert: (m1 intoWorldCount = 2).
	self assert: (m1 outOfWorldCount = 1).
	self assert: (m2 intoWorldCount = 2).
	self assert: (m2 outOfWorldCount = 1).

	"delete it"
	morph delete.
	self assert: (m1 intoWorldCount = 2).
	self assert: (m1 outOfWorldCount = 2).
	self assert: (m2 intoWorldCount = 2).
	self assert: (m2 outOfWorldCount = 2).

]

{ #category : #'testing - into/outof world' }
MorphTest >> testIntoWorldDeleteOutOfWorld [
	| m1 m2 |
	"Create the guys"
	m1 := TestInWorldMorph new.
	m2 := TestInWorldMorph new.
	self assert: (m1 intoWorldCount = 0).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 0).
	self assert: (m2 outOfWorldCount = 0).

	morph addMorphFront: m1.
	m1 addMorphFront:  m2.
	self assert: (m1 intoWorldCount = 0).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 0).
	self assert: (m2 outOfWorldCount = 0).

	morph openInWorld.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 0).

	morph delete.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 1).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 1).
	
]

{ #category : #'testing - into/outof world' }
MorphTest >> testIntoWorldTransferToNewGuy [
	| m1 m2 |
	"Create the guys"
	m1 := TestInWorldMorph new.
	m2 := TestInWorldMorph new.
	self assert: (m1 intoWorldCount = 0).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 0).
	self assert: (m2 outOfWorldCount = 0).

	morph addMorphFront: m1.
	m1 addMorphFront:  m2.
	self assert: (m1 intoWorldCount = 0).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 0).
	self assert: (m2 outOfWorldCount = 0).

	morph openInWorld.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 0).

	morph addMorphFront: m2.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 0).

	morph addMorphFront: m1.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 0).

	m2 addMorphFront: m1.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 0).

	morph delete.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 1).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 1).

]

{ #category : #'testing - classification' }
MorphTest >> testIsMorph [
	self assert: (morph isMorph).
]

{ #category : #'testing - initialization' }
MorphTest >> testOpenInWorld [
	morph openInWorld
]
