Class {
	#name : #MalHalTest,
	#superclass : #TestCase,
	#instVars : [
		'hal'
	],
	#category : #'Moose-Algos-Graph-Tests'
}

{ #category : #running }
MalHalTest >> setUp [
	super setUp.
	hal := MalHal new
]

{ #category : #tests }
MalHalTest >> testComplexCycle [
	(MalGraphFixture complexCycleGraph: hal) run.
	self assert: (hal findNode: $a) hNumber equals: 0.
	self assert: (hal findNode: $b) hNumber equals: 3.
	self assert: (hal findNode: $c) hNumber equals: 2.
	self assert: (hal findNode: $d) hNumber equals: 2.
	self assert: (hal findNode: $e) hNumber equals: 4.
	self assert: (hal findNode: $f) hNumber equals: 2.
	self assert: (hal findNode: $g) hNumber equals: 1.
	self assert: (hal findNode: $h) hNumber equals: 1.
	hal showLevels
]

{ #category : #tests }
MalHalTest >> testCycle [
	(MalGraphFixture cycleGraph: hal) run.
	self assert: (hal findNode: $a) hNumber equals: 1.
	self assert: (hal findNode: $b) hNumber equals: 1.
	self assert: (hal findNode: $c) hNumber equals: 1.
	self assert: (hal findNode: $d) hNumber equals: 0.
	hal showLevels
]

{ #category : #tests }
MalHalTest >> testModuleGraph [
	(MalGraphFixture moduleGraph: hal) run.
	self assert: (hal findNode: 'v1') hNumber equals: 0.
	self assert: (hal findNode: 'v2') hNumber equals: 1.
	self assert: (hal findNode: 'v3') hNumber equals: 1.
	self assert: (hal findNode: 'v4') hNumber equals: 1.
	self assert: (hal findNode: 'v5') hNumber equals: 2.
	self assert: (hal findNode: 'v6') hNumber equals: 3.
	self assert: (hal findNode: 'v7') hNumber equals: 3.
	self assert: (hal findNode: 'v8') hNumber equals: 4.
	self assert: (hal findNode: 'v9') hNumber equals: 5.
	self assert: (hal findNode: 'v10') hNumber equals: 6.
	self assert: (hal findNode: 'v11') hNumber equals: 6.
	hal showLevels
]

{ #category : #tests }
MalHalTest >> testModuleGraph2 [
	(MalGraphFixture moduleGraph2: hal) run.
	self assert: (hal findNode: $u) hNumber equals: 0.
	self assert: (hal findNode: $w) hNumber equals: 1.
	self assert: (hal findNode: $a) hNumber equals: 2.
	self assert: (hal findNode: $c) hNumber equals: 2.
	self assert: (hal findNode: $v) hNumber equals: 3.
	self assert: (hal findNode: $d) hNumber equals: 4.
	self assert: (hal findNode: $z) hNumber equals: 5.
	self assert: (hal findNode: $b) hNumber equals: 6.
	hal showLevels
]

{ #category : #tests }
MalHalTest >> testNestedCycle [
	(MalGraphFixture nestedCycleGraph: hal) run.
	self assert: (hal findNode: $a) hNumber equals: 0.
	self assert: (hal findNode: $b) hNumber equals: 0.
	self assert: (hal findNode: $c) hNumber equals: 0.
	self assert: (hal findNode: $d) hNumber equals: 0.
	self assert: (hal findNode: $e) hNumber equals: 0.
	self assert: (hal findNode: $f) hNumber equals: 1.
	self assert: (hal findNode: $g) hNumber equals: 1.
	self assert: (hal findNode: $h) hNumber equals: 1.
	self assert: (hal findNode: $i) hNumber equals: 1.
	hal showLevels
]

{ #category : #tests }
MalHalTest >> testSimpleGraph [
	(MalGraphFixture simpleGraph: hal) run.
	self assert: (hal findNode: $a) hNumber equals: 0.
	self assert: (hal findNode: $d) hNumber equals: 0.
	self assert: (hal findNode: $b) hNumber equals: 1.
	self assert: (hal findNode: $c) hNumber equals: 2.
	hal showLevels
]
