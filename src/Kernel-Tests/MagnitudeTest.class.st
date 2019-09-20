"
A MagnitudeTest is a test class for testing the behavior of Magnitude
"
Class {
	#name : #MagnitudeTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Numbers'
}

{ #category : #test }
MagnitudeTest >> testClampLowHigh [
	self assert: (10 clampLow: 12 high: 20) equals: 12.
	self assert: (13 clampLow: 12 high: 20) equals: 13.
	self assert: (12 clampLow: 12 high: 20) equals: 12.
	self assert: (100 clampLow: 12 high: 20) equals: 20
]

{ #category : #test }
MagnitudeTest >> testMinMax [
	self assert: (10 min: 20 max: 12) equals: 12.
	self assert: (13  min: 20 max: 12) equals: 13.
	self assert: (12 min: 20 max: 12) equals: 12.
	self assert: (100  min: 20 max: 12) equals: 20
]
