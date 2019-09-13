"
ZdcIOBufferTests holds unit tests for ZdcIOBuffer.
"
Class {
	#name : #ZdcIOBufferTest,
	#superclass : #TestCase,
	#category : #'Zodiac-Tests'
}

{ #category : #testing }
ZdcIOBufferTest >> testAdvanceReadPointer [
	| ioBuffer offset |
	ioBuffer := ZdcIOBuffer on: (ByteArray new: 10).
	3 timesRepeat: [ ioBuffer nextPut: 0 ].
	1 to: 6 do: [ :each | ioBuffer nextPut: each ].
	3 timesRepeat: [ ioBuffer next ].
	offset := ioBuffer contentsStart.
	self assert: ioBuffer availableForReading equals: 6.
	1 to: 6 do: [ :each | 
		self assert: (ioBuffer buffer at: offset + each - 1) equals: each ].
	ioBuffer advanceReadPointer: 6.
	self assert: ioBuffer availableForReading equals: 0.
	ioBuffer compact.
	self assert: ioBuffer isEmpty     
	
]

{ #category : #testing }
ZdcIOBufferTest >> testAdvanceWritePointer [
	| ioBuffer offset |
	ioBuffer := ZdcIOBuffer on: (ByteArray new: 10).
	self assert: ioBuffer isEmpty.
	offset := ioBuffer freeSpaceStart.
	1 to: 6 do: [ :each |
		ioBuffer buffer at: offset + each - 1 put: each ].
	ioBuffer advanceWritePointer: 6.
	self deny: ioBuffer isEmpty.
	self assert: ioBuffer availableForReading equals: 6.
	self assert: ioBuffer contents equals: #[1 2 3 4 5 6]
	
]

{ #category : #testing }
ZdcIOBufferTest >> testBivalentReading [
	| data ioBuffer buffer |
	data := 'some string'.
	
	ioBuffer := ZdcIOBuffer on: data.
	ioBuffer advanceWritePointer: data size.
	buffer := ByteArray new: data size.
	ioBuffer readInto: buffer startingAt: 1 count: data size.
	self assert: buffer equals: data asByteArray.
	
	ioBuffer := ZdcIOBuffer on: data asByteArray.
	ioBuffer advanceWritePointer: data size.
	buffer := String new: data size.
	ioBuffer readInto: buffer startingAt: 1 count: data size.
	self assert: buffer equals: data.

]

{ #category : #testing }
ZdcIOBufferTest >> testBivalentWriting [
	| data ioBuffer |
	data := 'some string'.
	
	ioBuffer := ZdcIOBuffer on: (String new: data size).
	ioBuffer next: data size putAll: data asByteArray startingAt: 1.
	self assert: ioBuffer contents equals: data.
	
	ioBuffer := ZdcIOBuffer on: (ByteArray new: data size).
	ioBuffer next: data size putAll: data startingAt: 1.
	self assert: ioBuffer contents equals: data asByteArray
]

{ #category : #testing }
ZdcIOBufferTest >> testCompact [
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (ByteArray new: 10).
	1 to: 6 do: [ :each | ioBuffer nextPut: each ].
	3 timesRepeat: [ ioBuffer next ].
	ioBuffer compact.
	self assert: ioBuffer availableForReading equals: 3.
	self assert: ioBuffer availableForWriting equals: 7.
	3 timesRepeat: [ ioBuffer next ].
	self assert: ioBuffer gapAtFront equals: 3.
	ioBuffer compact.
	self assert: ioBuffer isEmpty
	
		
]

{ #category : #testing }
ZdcIOBufferTest >> testCompactAtBeginning [
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (ByteArray new: 10).
	ioBuffer compact.
	self assert: ioBuffer isEmpty.
	self assert: ioBuffer availableForWriting equals: 10.
	1 to: 6 do: [ :each | ioBuffer nextPut: each ].
	ioBuffer compact.
	self assert: ioBuffer availableForReading equals: 6.
	self assert: ioBuffer availableForWriting equals: 4.
	self assert: ioBuffer gapAtFront isZero
]

{ #category : #testing }
ZdcIOBufferTest >> testCompactAtEnd [
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (ByteArray new: 10).
	10 timesRepeat: [ ioBuffer nextPut: 1 ].
	self assert: ioBuffer gapAtFront isZero.
	10 timesRepeat: [ ioBuffer next ].
	self assert: ioBuffer isFull.
	self assert: ioBuffer gapAtFront equals: 10.
	ioBuffer compact.
	self assert: ioBuffer isEmpty
	
		
]

{ #category : #testing }
ZdcIOBufferTest >> testCompactDoNothing [
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (ByteArray new: 10).
	self assert: ioBuffer isEmpty.
	self assert: ioBuffer availableForReading equals: 0.
	self assert: ioBuffer availableForWriting equals: 10.
	ioBuffer compact.
	self assert: ioBuffer isEmpty.
	self assert: ioBuffer availableForReading equals: 0.
	self assert: ioBuffer availableForWriting equals: 10.	    
	1 to: 6 do: [ :each | ioBuffer nextPut: each ].	
	self deny: ioBuffer isEmpty.
	self assert: ioBuffer availableForReading equals: 6.
	self assert: ioBuffer availableForWriting equals: 4.
	ioBuffer compact.
	self deny: ioBuffer isEmpty.
	self assert: ioBuffer availableForReading equals: 6.
	self assert: ioBuffer availableForWriting equals: 4.
]

{ #category : #testing }
ZdcIOBufferTest >> testNextPutAllStartingAt [
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (String new: 12).
	ioBuffer next: 4 putAll: 'abcd' startingAt: 1.
	ioBuffer next: 4 putAll: 'xxxxabcdyyyy' startingAt: 5.
	ioBuffer next: 4 putAll: '--abcd' startingAt: 3.
	self assert: ioBuffer contents equals: 'abcdabcdabcd'
]

{ #category : #testing }
ZdcIOBufferTest >> testReadIntoStartingAtCount [
	| data ioBuffer string |
	data := 'abcdefghijkl'.
	ioBuffer := ZdcIOBuffer on: data.
	ioBuffer advanceWritePointer: 12.
	string := String new: 12.
	ioBuffer readInto: string startingAt: 1 count: 4.
	ioBuffer readInto: string startingAt: 5 count: 4.
	ioBuffer readInto: string startingAt: 9 count: 4.
	self assert: string equals: data
]

{ #category : #testing }
ZdcIOBufferTest >> testReading [
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (ByteArray new: 10).
	1 to: 6 do: [ :each | ioBuffer nextPut: each ].
	1 to: 3 do: [ :each | self assert: ioBuffer next equals: each ].
	self assert: ioBuffer contents equals: #[4 5 6].
	4 to: 6 do: [ :each | self assert: ioBuffer next equals: each ].
	self assert: ioBuffer isEmpty.
	self should: [ ioBuffer next ] raise: Error
]

{ #category : #testing }
ZdcIOBufferTest >> testString [
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (String new: 10).
	($a to: $f) do: [ :each | ioBuffer nextPut: each ].
	self assert: ioBuffer contents equals: 'abcdef'.
	($a to: $f) do: [ :each | self assert: ioBuffer next equals: each ]
	
	
]

{ #category : #testing }
ZdcIOBufferTest >> testWriting [
	| ioBuffer |
	ioBuffer := ZdcIOBuffer on: (ByteArray new: 10).
	self assert: ioBuffer isEmpty.
	self assert: ioBuffer size isZero.
	self assert: ioBuffer availableForWriting equals: 10.
	1 to: 6 do: [ :each | ioBuffer nextPut: each ].
	self assert: ioBuffer contents equals: #[1 2 3 4 5 6].
	self assert: ioBuffer size equals: 6.
	self assert: ioBuffer availableForWriting equals: 4.
	7 to: 10 do: [ :each | ioBuffer nextPut: each ].
	self assert: ioBuffer isFull.
	self should: [ ioBuffer nextPut: 1 ] raise: Error
]
