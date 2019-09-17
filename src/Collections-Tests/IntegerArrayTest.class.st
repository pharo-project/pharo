"
SUnit tests for integer arrays
"
Class {
	#name : #IntegerArrayTest,
	#superclass : #TestCase,
	#category : #'Collections-Tests-Arrayed'
}

{ #category : #tests }
IntegerArrayTest >> testAt [
	| array |
	array := IntegerArray new: 5 withAll: 2.
	self assert: (array at: 3) equals: 2.
	
	array at: 3 put: 5.
	self assert: (array at: 3) equals: 5

]

{ #category : #tests }
IntegerArrayTest >> testPutAllNegative [
	| array |
	array := IntegerArray new: 2.
	array  atAllPut: -1000.
	self assert: (array at: 2) equals: -1000
]

{ #category : #tests }
IntegerArrayTest >> testPutNegative [
	| array |
	array := IntegerArray new: 2.
	array at: 2 put: -1000.
	self assert: (array at: 2) equals: -1000
]
