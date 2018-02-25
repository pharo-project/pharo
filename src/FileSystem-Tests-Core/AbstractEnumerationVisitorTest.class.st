"
SUnit tests for AbstractEnumerationVisitor
"
Class {
	#name : #AbstractEnumerationVisitorTest,
	#superclass : #SingleTreeTest,
	#category : #'FileSystem-Tests-Core'
}

{ #category : #'as yet unclassified' }
AbstractEnumerationVisitorTest class >> isAbstract [
	^ self name = #AbstractEnumerationVisitorTest
]

{ #category : #'as yet unclassified' }
AbstractEnumerationVisitorTest >> assertEntries: references are: expected [
	| strings |
	self assert: references isArray.
	references do: [ :ea | self assert: ea class = MemoryDirectoryEntry ].
	strings := references collect: [ :ea | ea reference pathString ].
	self assert: strings equals: expected
]

{ #category : #'as yet unclassified' }
AbstractEnumerationVisitorTest >> root [
	^ filesystem / 'alpha'
]

{ #category : #running }
AbstractEnumerationVisitorTest >> setUp [
	super setUp.
	self setUpGreek.
]
