"
A ShLayoutChangeDetectorTest is a test class for testing the behavior of ShLayoutChangeDetector
"
Class {
	#name : 'ShLayoutChangeDetectorTest',
	#superclass : 'ShAbstractChangeDetectorTest',
	#category : 'Shift-Changes-Tests',
	#package : 'Shift-Changes-Tests'
}

{ #category : 'tests' }
ShLayoutChangeDetectorTest >> testChangeOfLayoutIsDetected [

	newBuilder layoutClass: ImmediateLayout.

	self denyEmpty: self newComparer compareClass.
	self assertChangeAreDetected
]

{ #category : 'tests' }
ShLayoutChangeDetectorTest >> testNoChangeOfLayoutDetected [

	newBuilder layoutClass: FixedLayout.

	self assertEmpty: self newComparer compareClass.
	self assertChangeArentDetected
]
