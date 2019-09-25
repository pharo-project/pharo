"
A MagnitudeTest is a test class for testing the behavior of Magnitude
"
Class {
	#name : #MagnitudeTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Numbers'
}

{ #category : #test }
MagnitudeTest >> testBeBetweenAnd [
	"inside"
	self assert: (10 beBetween: 5 and: 20) equals: 10.
	self assert: (10 beBetween: 20 and: 5) equals: 10.
	"equal to one end interval"
	self assert: (5 beBetween: 5 and: 20) equals: 5.
	self assert: (5 beBetween: 20 and: 5) equals: 5.
	self assert: (20 beBetween: 5 and: 20) equals: 20.
	self assert: (20 beBetween: 20 and: 5) equals: 20.
	"bellow"
	self assert: (1 beBetween: 5 and: 20) equals: 5.
	self assert: (1 beBetween: 20 and: 5) equals: 5.
	"above"
	self assert: (30 beBetween: 5 and: 20) equals: 20.
	self assert: (30 beBetween: 20 and: 5) equals: 20.
]

{ #category : #test }
MagnitudeTest >> testMinMax [
	self assert: (10 min: 20 max: 12) equals: 12.
	self assert: (13  min: 20 max: 12) equals: 13.
	self assert: (12 min: 20 max: 12) equals: 12.
	self assert: (100  min: 20 max: 12) equals: 20
]
