Class {
	#name : 'FTTableMorphTest',
	#superclass : 'TestCase',
	#traits : 'SpTMorphicUIRedrawer',
	#classTraits : 'SpTMorphicUIRedrawer classTrait',
	#instVars : [
		'table'
	],
	#category : 'Morphic-Widgets-FastTable-Tests-Configuration',
	#package : 'Morphic-Widgets-FastTable-Tests',
	#tag : 'Configuration'
}

{ #category : 'running' }
FTTableMorphTest >> setUp [
	super setUp.
	table := FTTableMorph new.
	table extent: 800@600.
	self initializeSemaphore
]

{ #category : 'running' }
FTTableMorphTest >> tearDown [
	table delete.
	super tearDown
]

{ #category : 'tests' }
FTTableMorphTest >> testCanAlternateRowColors [
	| rows oddRows oddColor evenRows evenColor |
	table
		hideColumnHeaders;
		alternateRowsColor: true.
	table openInWorld.
	table dataSource: (FTSimpleDataSource elements: #(1 2 3 4 5)).
	self waitUntilUIRedrawed.
	rows := table container submorphs.

	oddRows := rows withIndexSelect: [ :row : index | index odd ].
	oddColor := oddRows first color.
	evenRows := rows withIndexSelect: [ :row : index | index even ].
	evenColor := evenRows first color.

	self
		assert: (oddRows collect: [ :e | e color ] as: Set) size
		equals: 1.
	self
		assert: (evenRows collect: [ :e | e color ] as: Set) size
		equals: 1.
	self deny: oddColor equals: evenColor
]
