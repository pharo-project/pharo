"
SUnit tests for AbstractEnumerationVisitor
"
Class {
	#name : #AbstractEnumerationVisitorTest,
	#superclass : #SingleTreeTest,
	#category : #'FileSystem-Tests-Core-Visitors'
}

{ #category : #testing }
AbstractEnumerationVisitorTest class >> isAbstract [
	^ self name = #AbstractEnumerationVisitorTest
]

{ #category : #utilities }
AbstractEnumerationVisitorTest >> assertEntries: references are: expected [
	| strings |
	self assert: references isArray.
	references do: [ :ea | self assert: ea class equals: MemoryDirectoryEntry ].
	strings := references collect: [ :ea | ea reference pathString ].
	self assert: strings equals: expected
]

{ #category : #accessing }
AbstractEnumerationVisitorTest >> root [
	^ filesystem / 'alpha'
]

{ #category : #running }
AbstractEnumerationVisitorTest >> setUp [
	super setUp.
	self setUpGreek
]
