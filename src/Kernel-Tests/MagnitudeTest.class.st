"
A MagnitudeTest is a test class for testing the behavior of Magnitude
"
Class {
	#name : #MagnitudeTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Numbers'
}

{ #category : #test }
MagnitudeTest >> testBetweenAnd [
	self assert: (3 between: 0 and: 5).
	self assert: (5.0 between: 5.0 and: 5.0).
	self assert: (Date today between: Date today +1 and: Date today +3) not
]

{ #category : #test }
MagnitudeTest >> testCompareWithIfLesserIfEqualIfGreater [

	self assert: ($b compareWith: $a ifLesser: [ 1 ] ifEqual: [ 2 ] ifGreater: [3]) equals: 3.
	self assert: ($a compareWith: $b ifLesser: [ 1 ] ifEqual: [ 2 ] ifGreater: [3]) equals: 1.
	self assert: ($b compareWith: $b ifLesser: [ 1 ] ifEqual: [ 2 ] ifGreater: [3]) equals: 2.
]

{ #category : #test }
MagnitudeTest >> testMax [

	| huge |
	self assert: ( 2 max: 3) equals: 3.
	huge := Float fmax asInteger + 1.
	self assert: (Float fmax max: huge) equals: huge.
	self assert: (1/3 max: 0.33333333333333333333333333333333333333333) equals: 1/3
]

{ #category : #test }
MagnitudeTest >> testMin [

	| huge aThirdFloat |
	self assert: ( 2 min: 3) equals: 2.
	huge := Float fmax asInteger + 1.
	self assert: (Float fmax min: huge) equals: Float fmax.
	aThirdFloat := 0.33333333333333333333333333333333333333333.
	self assert: (1/3 min: aThirdFloat) equals: aThirdFloat
]

{ #category : #test }
MagnitudeTest >> testMinMax [

	self assert: (42 min: 50 max: 40) equals: 42.
	self assert: (42 min: 50 max: 45) equals: 45.
	self assert: (42 min: 40 max: 30) equals: 40	
]

{ #category : #test }
MagnitudeTest >> testThreeWayCompareTo [

	self assert: ($b threeWayCompareTo: $a) equals: 1.
	self assert: ($a threeWayCompareTo: $b) equals: -1.
	self assert: ($b threeWayCompareTo: $b ) equals: 0.
]
