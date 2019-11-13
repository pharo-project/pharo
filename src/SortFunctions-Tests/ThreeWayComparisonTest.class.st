"
Unit test for three way comparision
"
Class {
	#name : #ThreeWayComparisonTest,
	#superclass : #TestCase,
	#category : #'SortFunctions-Tests'
}

{ #category : #tests }
ThreeWayComparisonTest >> testBooleans [
	self assert: (true threeWayCompareTo: false) equals: 1.
	self assert: (false threeWayCompareTo: true) equals: -1.
	self assert: (true threeWayCompareTo: true) equals: 0.
	self assert: (false threeWayCompareTo: false) equals: 0
]

{ #category : #tests }
ThreeWayComparisonTest >> testFloats [

	| a b c d |
	a := 13.1.
	b := 18.9.
	c := 18.9.
	d := -7.2.
	self assert: (b threeWayCompareTo: c) isZero.
	self assert: (a threeWayCompareTo: b) equals: -1.
	self assert: (b threeWayCompareTo: a) equals: 1.
	self assert: (a threeWayCompareTo: d) equals: 1.
	self assert: (d threeWayCompareTo: a) equals: -1
]

{ #category : #tests }
ThreeWayComparisonTest >> testIntegers [
	self assert: (42 threeWayCompareTo: 42) equals: 0.
	self assert: (1 threeWayCompareTo: 42) equals: -1.
	self assert: (100 threeWayCompareTo: 42) equals: 1
]

{ #category : #tests }
ThreeWayComparisonTest >> testStrings [

	| a b c d |
	a := 'xc'.
	b := 'xd'.
	c := 'xd'.
	d := 'xa'.
	self assert: (b threeWayCompareTo: c) isZero.
	self assert: (a threeWayCompareTo: b) equals: -1.
	self assert: (b threeWayCompareTo: a) equals: 1.
	self assert: (a threeWayCompareTo: d) equals: 1.
	self assert: (d threeWayCompareTo: a) equals: -1
]
