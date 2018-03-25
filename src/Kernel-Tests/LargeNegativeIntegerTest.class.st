"
SUnit tests for large negative integers
"
Class {
	#name : #LargeNegativeIntegerTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Numbers'
}

{ #category : #tests }
LargeNegativeIntegerTest >> testEmptyTemplate [
	"Check that an uninitialized instance behaves reasonably."

	| i |
	i := LargeNegativeInteger new: 4.
	self assert: i size = 4.
	self assert: i printString = '-0'.
	self assert: i normalize = 0
]

{ #category : #'tests-printing' }
LargeNegativeIntegerTest >> testStoreOn [
	| integer |
	integer := SmallInteger minVal - 1.
	
	"Store on adds a space before a negative number to avoid mistaking it with a - message"
	self
		assert: integer class equals: LargeNegativeInteger;
		assert: (String streamContents: [ :s | integer storeOn: s ]) equals: ' ', integer asString
]
