"
SUnit tests for null streams
"
Class {
	#name : #NullStreamTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'stream'
	],
	#category : 'Collections-Tests-Streams'
}

{ #category : #running }
NullStreamTest >> setUp [
	super setUp.
	stream := NullStream new.
]

{ #category : #tests }
NullStreamTest >> testCounting [
	| expectedPosition |
	expectedPosition := 0.
	self assert: stream position equals: expectedPosition.
	
	stream nextPut: $a.
	expectedPosition := expectedPosition + 1.
	self assert: stream position equals: expectedPosition.
	
	stream nextPutAll: 'bcd'.
	expectedPosition := expectedPosition + 3.
	self assert: stream position equals: expectedPosition.
	
	stream cr.
	expectedPosition := expectedPosition + 1.
	self assert: stream position equals: expectedPosition.
	
	stream lf.
	expectedPosition := expectedPosition + 1.
	self assert: stream position equals: expectedPosition.
	
	stream space.
	expectedPosition := expectedPosition + 1.
	self assert: stream position equals: expectedPosition.
	
	stream tab.
	expectedPosition := expectedPosition + 1.
	self assert: stream position equals: expectedPosition.
	
	stream next: 4 putAll: 'abcdefgh' startingAt: 3.
	expectedPosition := expectedPosition + 4.
	self assert: stream position equals: expectedPosition.
]

{ #category : #tests }
NullStreamTest >> testNextPutAllEmpty [
	self assert: stream position equals: 0.
	self assert: stream peekLast equals: nil.
	stream nextPutAll: ''.
	self assert: stream position equals: 0.
	self assert: stream peekLast equals: nil.
]

{ #category : #tests }
NullStreamTest >> testPeekLast [
	self assert: stream peekLast equals: nil.
	stream nextPut: $a.
	self assert: stream peekLast equals: $a.
	stream nextPutAll: 'bcd'.
	self assert: stream peekLast equals: $d.
	stream cr.
	self assert: stream peekLast equals: Character cr.
	stream lf.
	self assert: stream peekLast equals: Character lf.
	stream space.
	self assert: stream peekLast equals: Character space.
	stream tab.
	self assert: stream peekLast equals: Character tab.
	stream next: 4 putAll: 'abcdefgh' startingAt: 3.
	self assert: stream peekLast equals: $f.
]
