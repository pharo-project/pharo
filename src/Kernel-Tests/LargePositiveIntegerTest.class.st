"
SUnit tests for large positive integers
"
Class {
	#name : #LargePositiveIntegerTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Numbers'
} 

{ #category : #tests }
LargePositiveIntegerTest >> testBitShift [

	"Check bitShift from and back to SmallInts"
	
	1 to: 257 do: [:i | self should: [((i bitShift: i) bitShift: 0-i) == i]].
]

{ #category : #tests }
LargePositiveIntegerTest >> testEmptyTemplate [

	"Check that an uninitialized instance behaves reasonably."

	| i |
	i := LargePositiveInteger new: 4.
	self assert: i size = 4.
	self assert: i printString = '0'.
	self assert: i normalize = 0
]

{ #category : #tests }
LargePositiveIntegerTest >> testLargeSqrtFloor [
	"This test fails if a careless implementation naivly factors out the power of two (remove the trailing zeroes up to lowBit).
	This was the case in a previous implementation, so this is a non regression test."

	| large root |
	large := (SmallInteger maxVal << 100 + 1) << 100.
	root := large sqrtFloor.
	self assert: root squared <= large.
	 self assert: (root+1) squared > large.
]

{ #category : #tests }
LargePositiveIntegerTest >> testMultDicAddSub [

	| n f f1 |	
	n := 100.
	f := 100 factorial.
	f1 := f*(n+1).
	n timesRepeat: [f1 := f1 - f].
	self assert: f1 equals: f. 

	n timesRepeat: [f1 := f1 + f].
	self assert: f1 // f equals: n+1. 
	self assert: f1 negated equals: (Number readFrom: '-' , f1 printString)
]

{ #category : #tests }
LargePositiveIntegerTest >> testNormalize [
	"Check normalization and conversion to/from SmallInts"

	self assert: ((SmallInteger maxVal + 1 - 1) == SmallInteger maxVal).
	self assert: (SmallInteger maxVal + 3 - 6) == (SmallInteger maxVal-3).
	self should: ((SmallInteger minVal - 1 + 1) == SmallInteger minVal).
	self assert: (SmallInteger minVal - 3 + 6) == (SmallInteger minVal+3).
]

{ #category : #'tests-printing' }
LargePositiveIntegerTest >> testStoreOn [
	| integer |
	integer := 1073741824.
	self
		assert: integer class equals: LargePositiveInteger;
		assert: (String streamContents: [ :s | integer storeOn: s ]) equals: '1073741824'.
]
