"
This is the unit test for the class FilteringStream
"
Class {
	#name : #FilteringStreamTest,
	#superclass : #TestCase,
	#category : #'Collections-Streams-Tests'
}

{ #category : #tests }
FilteringStreamTest >> testWithCharacters [
	|f|
	f := FilteringStream on: (String new writeStream) with: [:x | x].
	f nextPut: $a.
	self assert: f contents size equals: 1. 
	self assert: (f contents at:1) equals: $a.
]

{ #category : #tests }
FilteringStreamTest >> testWithNumbers [
	|f|
	f := FilteringStream on: (WriteStream with: LinkedList new) with: [:x | x + 10].
	f nextPut: 23.
	self assert: f contents size equals: 1. 
	self assert: (f contents at:1) equals: 33.
]
