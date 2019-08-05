"
SUnit Test for ShortIntegerArray
"
Class {
	#name : #ShortIntegerArrayTest,
	#superclass : #TestCase,
	#instVars : [
		'shortIntegerArray'
	],
	#category : #'Balloon-Tests-Collections'
}

{ #category : #running }
ShortIntegerArrayTest >> setUp [ 

	super setUp.
	shortIntegerArray := ShortIntegerArray with: 4 with: 4+256 with: 2 with: 1+(256*10).

]

{ #category : #tests }
ShortIntegerArrayTest >> testAt [

	self assert: 2 equals: (shortIntegerArray at: 3)
	
]

{ #category : #tests }
ShortIntegerArrayTest >> testAtPut [

	shortIntegerArray at: 3 put: 100.
	self assert: 100 equals: (shortIntegerArray at: 3)
	
]

{ #category : #tests }
ShortIntegerArrayTest >> testSetUp [

	self assert: 4 equals: (shortIntegerArray at:1) . 
	self assert: 4+256 equals: (shortIntegerArray at:2).
	self assert: 2 equals: (shortIntegerArray at:3).
	self assert: 1+(256*10) equals: (shortIntegerArray at:4).

]

{ #category : #tests }
ShortIntegerArrayTest >> testSwapBytePairs [

	shortIntegerArray swapBytePairsHacky.
	self assert: 4*256 equals: (shortIntegerArray at:1). 
	self assert: 4*256 + 1 equals: (shortIntegerArray at:2).
	self assert: 2*256 equals: (shortIntegerArray at:3).
	self assert: 1*256 + 10 equals: (shortIntegerArray at:4).

]

{ #category : #tests }
ShortIntegerArrayTest >> testWriteBigEndianOn [

	|stream|
	stream := WriteStream on: Array new.
	shortIntegerArray write: (12 bitShift: 24) + (34 bitShift: 16) + (56 bitShift: 8) + 78 bigEndianOn: stream.
	self assert: #(12 34 56 78) equals: stream contents
]

{ #category : #tests }
ShortIntegerArrayTest >> testWriteLittleEndianOn [

	|stream|
	stream := WriteStream on: Array new.
	shortIntegerArray write: (12 bitShift: 24) + (34 bitShift: 16) + (56 bitShift: 8) + 78 littleEndianOn: stream.
	self assert: #(56 78 12 34) equals: stream contents
]
