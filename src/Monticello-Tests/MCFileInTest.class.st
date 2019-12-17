Class {
	#name : #MCFileInTest,
	#superclass : #MCTestCase,
	#instVars : [
		'stream',
		'expected',
		'diff'
	],
	#category : #'Monticello-Tests-IO'
}

{ #category : #testing }
MCFileInTest >> alterInitialState [
	self mockClassA touchCVar
]

{ #category : #testing }
MCFileInTest >> assertFileOutFrom: writerClass canBeFiledInWith: aBlock [
	(writerClass on: stream) writeSnapshot: self mockSnapshot.
	self alterInitialState.
	self assertSuccessfulLoadWith: aBlock.
	self mockPackage unload.
	self assertSuccessfulLoadWith: aBlock.

]

{ #category : #testing }
MCFileInTest >> assertInitializersCalled [
	| cvar |
	cvar := self mockClassA cVar.
	self assert: cvar equals: #initialized
]

{ #category : #testing }
MCFileInTest >> assertInitializersOrder [
	| initializationOrder |
	initializationOrder := self mockClassA initializationOrder.
	self assert: initializationOrder equals: 2
]

{ #category : #asserting }
MCFileInTest >> assertNoChange [
	| actual |
	actual := MCSnapshotResource takeSnapshot.
	diff := actual patchRelativeToBase: expected.
	self assertEmpty: diff
]

{ #category : #testing }
MCFileInTest >> assertSuccessfulLoadWith: aBlock [
	stream reset.
	aBlock value.
	self assertNoChange.
	self assertInitializersCalled.
	self assertInitializersOrder
]

{ #category : #running }
MCFileInTest >> setUp [
	super setUp.
	expected := self mockSnapshot.
	stream := ReadWriteStream on: ''
]

{ #category : #running }
MCFileInTest >> tearDown [
	(diff isNil or: [diff isEmpty not])
		 ifTrue: [expected updatePackage: self mockPackage].
	MCDataStream initialize.
	super tearDown
]

{ #category : #testing }
MCFileInTest >> testStWriter [
	
	self
		assertFileOutFrom: MCStWriter
		canBeFiledInWith: [ CodeImporter evaluateReadStream: stream readStream].

]
