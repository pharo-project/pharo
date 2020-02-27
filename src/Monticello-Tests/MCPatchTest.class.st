Class {
	#name : #MCPatchTest,
	#superclass : #MCTestCase,
	#instVars : [
		'patch'
	],
	#category : #'Monticello-Tests-Base'
}

{ #category : #running }
MCPatchTest >> setUp [
	|rev1 rev2|
	super setUp.
	rev1 :=  MCSnapshotResource takeSnapshot.
	self change: #one toReturn: 2.
	rev2 :=  MCSnapshotResource takeSnapshot.
	patch := rev2 patchRelativeToBase: rev1.
	self change: #one toReturn: 1.
]

{ #category : #running }
MCPatchTest >> tearDown [
	self restoreMocks.
	super tearDown
]

{ #category : #tests }
MCPatchTest >> testPatchContents [
	self assert: patch operations size equals: 1.
	self assert: patch operations first isModification.
	self assert: patch operations first definition selector equals: #one
]
