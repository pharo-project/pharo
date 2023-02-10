Class {
	#name : #MathOperationnsExtensionsTest,
	#superclass : #TestCase,
	#category : #'Math-Operations-Extensions-Tests'
}

{ #category : #tests }
MathOperationnsExtensionsTest >> testCloseTo [

	self assert: (3 closeTo: 3.001 precision: 0.001).
	self assert: (0 closeTo: 0.001 precision: 0.001).
	self deny: (3.345 closeTo: 3.35 precision: 0.001)
]
