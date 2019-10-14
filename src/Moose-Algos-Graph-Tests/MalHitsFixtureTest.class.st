Class {
	#name : #MalHitsFixtureTest,
	#superclass : #TestCase,
	#instVars : [
		'hits'
	],
	#category : #'Moose-Algos-Graph-Tests'
}

{ #category : #running }
MalHitsFixtureTest >> setUp [
	super setUp.
	hits := MalHits new
]

{ #category : #tests }
MalHitsFixtureTest >> testComplexCycle [
	MalGraphFixture complexCycleGraph: hits.
	self shouldnt: [ hits run ] raise: Error
]

{ #category : #tests }
MalHitsFixtureTest >> testComplexWCycle [
	MalGraphFixture complexWeightedGraph: hits.
	self shouldnt: [ hits run ] raise: Error
]

{ #category : #tests }
MalHitsFixtureTest >> testCycle [
	MalGraphFixture cycleGraph: hits.
	self shouldnt: [ hits run ] raise: Error
]

{ #category : #tests }
MalHitsFixtureTest >> testNestedCycle [
	MalGraphFixture nestedCycleGraph: hits.
	self shouldnt: [ hits run ] raise: Error
]

{ #category : #tests }
MalHitsFixtureTest >> testSimple [
	MalGraphFixture simpleGraph: hits.
	self shouldnt: [ hits run ] raise: Error
]

{ #category : #tests }
MalHitsFixtureTest >> testSimpleW [
	MalGraphFixture simpleWeightedGraph: hits.
	self shouldnt: [ hits run ] raise: Error
]
