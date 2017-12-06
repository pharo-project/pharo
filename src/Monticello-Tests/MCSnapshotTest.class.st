Class {
	#name : #MCSnapshotTest,
	#superclass : #MCTestCase,
	#instVars : [
		'snapshot'
	],
	#category : #'Monticello-Tests-Snapshots'
}

{ #category : #running }
MCSnapshotTest >> setUp [
	super setUp.
	snapshot :=  self mockSnapshot.
]

{ #category : #tests }
MCSnapshotTest >> testCreation [
	|d|
	d :=  self mockSnapshot definitions.
	self assert: (d anySatisfy: [:ea | ea isClassDefinition and: [ea className = #MCMockClassA]]).
	self assert: (d anySatisfy: [:ea | ea isMethodDefinition and: [ea selector = #mockClassExtension]]).
	self assert: (d allSatisfy: [:ea | ea isClassDefinition not or: [ea category endsWith: 'Mocks']]).
	
]

{ #category : #tests }
MCSnapshotTest >> testInstanceReuse [
	| x m n y |
	
	self timeLimit: 300 seconds.
	
	x := (MCPackage new name: self mockCategoryName) snapshot.
	Smalltalk garbageCollect.
	n := MCDefinition allSubInstances size.
	y := (MCPackage new name: self mockCategoryName) snapshot.
	Smalltalk garbageCollect.
	m := MCDefinition allSubInstances size.
	self assert: m = n
]
