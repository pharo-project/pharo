"
SUnit tests for SparseLargeTable
"
Class {
	#name : 'SparseLargeTableTest',
	#superclass : 'TestCase',
	#category : 'Collections-Sequenceable-Tests',
	#package : 'Collections-Sequenceable-Tests'
}

{ #category : 'tests' }
SparseLargeTableTest >> testCopyAfterShareEqualChunksIsWritable [

	| table copy |
	table := SparseLargeTable new: 20 chunkSize: 10 arrayClass: ByteArray base: 1 defaultValue: 0.
	1 to: 20 do: [ :index | table at: index put: 1 ].
	table shareEqualChunks.
	copy := table copy.
	copy at: 5 put: 2.

	self assert: (copy at: 5) equals: 2.
	self assert: (copy at: 15) equals: 1.
	self assert: (table at: 5) equals: 1
]

{ #category : 'tests' }
SparseLargeTableTest >> testShareEqualChunks [

	| table |
	table := SparseLargeTable new: 40 chunkSize: 10 arrayClass: ByteArray base: 1 defaultValue: 0.
	1 to: 10 do: [ :index | table at: index put: index ].
	21 to: 30 do: [ :index | table at: index put: index - 20 ].
	table at: 35 put: 7.
	table shareEqualChunks.

	self assert: (table basicAt: 1) identicalTo: (table basicAt: 3).
	self assert: (table at: 25) equals: 5.
	self assert: (table at: 35) equals: 7.
	self assert: (table at: 15) equals: 0.
	self should: [ table at: 25 put: 1 ] raise: ModificationForbidden
]
