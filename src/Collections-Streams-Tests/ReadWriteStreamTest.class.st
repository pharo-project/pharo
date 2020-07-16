"
This is the unit test for the class ReadWriteStream.
Unit tests are a good way to exercise the
functionality of your system in a repeatable and
automatic manner. They are therefore recommended if
you plan to release anything. For more information,
see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org/)
	- the sunit class category
"
Class {
	#name : #ReadWriteStreamTest,
	#superclass : #ClassTestCase,
	#category : #'Collections-Streams-Tests-Base'
}

{ #category : #coverage }
ReadWriteStreamTest >> classToBeTested [
	
	^ ReadWriteStream
]

{ #category : #'accessing - defaults' }
ReadWriteStreamTest >> emptyByteStream [
	^ ReadWriteStream on: (ByteArray new: 4096).
]

{ #category : #'accessing - defaults' }
ReadWriteStreamTest >> emptyStream [
	^ ReadWriteStream on: (String new: 4096).
]

{ #category : #'accessing - defaults' }
ReadWriteStreamTest >> streamOnString [
	^ ReadWriteStream with: 'abcde'.
]

{ #category : #'tests - testing' }
ReadWriteStreamTest >> testAtEnd [
	| stream |
	stream := self emptyStream.
	self assert: stream atEnd.
	stream
		nextPut: $a;
		reset.
	self deny: stream atEnd
]

{ #category : #tests }
ReadWriteStreamTest >> testConstructionUsingWith [
	"Use the with: constructor."

	| aStream |
	aStream := ReadWriteStream with: #(1 2).
	self assert: (aStream contents = #(1 2)) description: 'Ensure correct initialization.'
]

{ #category : #tests }
ReadWriteStreamTest >> testContents [
	| stream |
	stream := self emptyStream.
	stream
		nextPutAll: 'abc';
		reset.
	self assert: stream contents equals: 'abc'.
	stream next: 2.
	self assert: stream contents equals: 'abc'.
	stream := self emptyByteStream.
	stream
		nextPutAll: 'abc' asByteArray;
		reset.
	self assert: stream contents equals: 'abc' asByteArray.
	stream next: 2.
	self assert: stream contents equals: 'abc' asByteArray
]

{ #category : #'tests - testing' }
ReadWriteStreamTest >> testIsEmpty [
	| stream |
	stream := self emptyStream.
	self assertEmpty: stream.
	stream nextPut: $a.
	self deny: stream isEmpty.
	stream reset.
	self deny: stream isEmpty.
	stream next.
	self deny: stream isEmpty.
]

{ #category : #tests }
ReadWriteStreamTest >> testNew [

	self should: [ ReadWriteStream new ] raise: Error.
]

{ #category : #tests }
ReadWriteStreamTest >> testNext [
	| stream |
	stream := self emptyStream.
	stream
		nextPutAll: 'abcd';
		reset.
	self assert: stream next equals: $a.
	self assert: (stream next: 0) equals: ''.
	self assert: (stream next: 1) equals: 'b'.
	self assert: (stream next: 2) equals: 'cd'.
	stream := self emptyByteStream.
	stream
		nextPutAll: 'abcd' asByteArray;
		reset.
	self assert: stream next equals: 97.
	self assert: (stream next: 0) equals: '' asByteArray.
	self assert: (stream next: 1) equals: 'b' asByteArray.
	self assert: (stream next: 2) equals: 'cd' asByteArray
]

{ #category : #tests }
ReadWriteStreamTest >> testNextPut [
	| stream |
	stream := self emptyStream.
	stream nextPut: $a.
	self assert: stream contents equals: 'a'.
	stream := self emptyByteStream.
	stream nextPut: 97.
	self assert: stream contents equals: 'a' asByteArray
]

{ #category : #tests }
ReadWriteStreamTest >> testNextPutAll [
	| stream |
	stream := self emptyStream.
	stream nextPutAll: 'abc'.
	self assert: stream contents equals: 'abc'.
	stream := self emptyStream.
	stream nextPutAll: #($a $b $c).
	self assert: stream contents equals: 'abc'.
	stream := self emptyByteStream.
	stream nextPutAll: #(97 98 99 ) asByteArray.
	self assert: stream contents equals: 'abc' asByteArray
]

{ #category : #tests }
ReadWriteStreamTest >> testPeek [
	| stream |
	stream := self emptyStream.
	self assert: stream peek isNil.
	stream := self emptyStream.
	stream
		nextPutAll: 'abcd';
		reset.
	self assert: stream peek equals: $a.
	stream := self emptyByteStream.
	stream
		nextPutAll: 'abcd' asByteArray;
		reset.
	self assert: stream peek equals: 97
]

{ #category : #tests }
ReadWriteStreamTest >> testPosition [
	| stream |
	stream := self emptyStream.
	stream
		nextPutAll: 'abc';
		reset.
	self assert: stream position equals: 0.
	stream next.
	self assert: stream position equals: 1.
	stream next.
	self assert: stream position equals: 2.
	stream next.
	self assert: stream position equals: 3.
	stream position: 1.
	self assert: stream position equals: 1.
	self assert: stream next equals: $b.
	stream position: 0.
	self assert: stream position equals: 0.
	self assert: stream next equals: $a.
	stream position: 3.
	self assert: stream atEnd.
	stream := self emptyByteStream.
	stream
		nextPutAll: 'abc' asByteArray;
		reset.
	self assert: stream position equals: 0.
	stream next.
	self assert: stream position equals: 1.
	stream next.
	self assert: stream position equals: 2.
	stream next.
	self assert: stream position equals: 3.
	stream position: 1.
	self assert: stream position equals: 1.
	self assert: stream next equals: 98.
	stream position: 0.
	self assert: stream position equals: 0.
	self assert: stream next equals: 97.
	stream position: 3.
	self assert: stream atEnd
]

{ #category : #tests }
ReadWriteStreamTest >> testReset [
	| stream |
	stream := self emptyStream.
	stream
		nextPutAll: 'abc';
		reset.
	stream next: 2.
	stream reset.
	self assert: stream next equals: $a.
	stream := self emptyStream.
	stream 
		nextPutAll: 'abc';
		reset.
	stream nextPutAll: 'def'.
	self assert: stream contents equals: 'def'.
	stream := self emptyByteStream.
	stream
		nextPutAll: 'abc' asByteArray;
		reset.
	stream next: 2.
	stream reset.
	self assert: stream next equals: 97
]

{ #category : #tests }
ReadWriteStreamTest >> testSkip [
	| stream |
	stream := self emptyStream.
	stream
		nextPutAll: 'abcd';
		reset.
	self assert: (stream skip: 2; peek) equals: $c.
	stream := self emptyByteStream.
	stream
		nextPutAll: 'abcd' asByteArray;
		reset.
	self assert: (stream skip: 2; peek) equals: 99
]

{ #category : #tests }
ReadWriteStreamTest >> testTab [
	| stream |
	stream := self emptyStream.
	stream tab.
	self assert: stream contents first equals: Character tab
]

{ #category : #tests }
ReadWriteStreamTest >> testUpTo [
	| stream |
	stream := self emptyStream.
	stream
		nextPutAll: 'abcd';
		reset.
	self assert: (stream upTo: $c) equals: 'ab'.
	self assert: stream next equals: $d.
	stream := self emptyStream.
	stream
		nextPutAll: 'abcd';
		reset.
	self assert: (stream upTo: $x) equals: 'abcd'.
	self assert: stream atEnd.
	stream := self emptyByteStream.
	stream
		nextPutAll: 'abcd' asByteArray;
		reset.
	self assert: (stream upTo: 99) equals: #(97 98) asByteArray.
	self assert: stream next equals: 100.
	stream := self emptyByteStream.
	stream
		nextPutAll: 'abcd' asByteArray;
		reset.
	self assert: (stream upTo: 120) equals: #(97 98 99 100) asByteArray.
	self assert: stream atEnd
]

{ #category : #tests }
ReadWriteStreamTest >> testUpToEnd [
	| stream |
	stream := self emptyStream.
	stream
		nextPutAll: 'abcd';
		reset.
	self assert: stream upToEnd equals: 'abcd'.
	self assert: stream atEnd.
	self assert: stream upToEnd equals: ''.
	self assert: stream atEnd.
	stream reset.
	stream upTo: $b.
	self assert: stream upToEnd equals: 'cd'.
	self assert: stream atEnd.
	stream := self emptyByteStream.
	stream
		nextPutAll: 'abcd' asByteArray;
		reset.
	self assert: stream upToEnd equals: #(97 98 99 100) asByteArray.
	self assert: stream atEnd.
	self assert: stream upToEnd equals: #() asByteArray.
	self assert: stream atEnd.
	stream reset.
	stream upTo: 98.
	self assert: stream upToEnd equals: #(99 100) asByteArray.
	self assert: stream atEnd
]
