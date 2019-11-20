"
A trait for test purposes
"
Class {
	#name : #WriteStreamTest,
	#superclass : #ClassTestCase,
	#category : #'Collections-Tests-Streams'
}

{ #category : #testing }
WriteStreamTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #coverage }
WriteStreamTest >> classToBeTested [

	^ self streamClass
]

{ #category : #'instance creation' }
WriteStreamTest >> newStream [
	^ self newStreamOn: String new
]

{ #category : #'instance creation' }
WriteStreamTest >> newStreamOn: aCollection [
	^ self streamClass on: aCollection
]

{ #category : #accessing }
WriteStreamTest >> streamClass [
	^ WriteStream
]

{ #category : #accessing }
WriteStreamTest >> string [
	^ 'testing'
]

{ #category : #accessing }
WriteStreamTest >> stringSize [
	^ self string size
]

{ #category : #tests }
WriteStreamTest >> testEnsureASpace [
	| stream |
	stream := self newStream.
	stream nextPutAll: 'this is a test'.
	stream ensureASpace.
	stream nextPutAll: 'for WriteStreamTest'.
	self assert: stream contents equals: 'this is a test for WriteStreamTest'.

	"Manually put a space and verify there are no 2 consecutive spaces"
	stream := self newStream.
	stream nextPutAll: 'this is a test '.
	stream ensureASpace.
	stream nextPutAll: 'for WriteStreamTest'.
	self assert: stream contents equals: 'this is a test for WriteStreamTest'
]

{ #category : #tests }
WriteStreamTest >> testEnsureASpace2 [
	| stream |
	stream := self newStream.
	stream ensureASpace.
	self assert: stream contents equals: ' '
]

{ #category : #tests }
WriteStreamTest >> testEnsureEndsWith [
	
	| stream |
	stream := self newStream.
	stream nextPutAll: 'this is a test'.
	stream ensureEndsWith: Character cr.
	stream nextPutAll: 'for WriteStreamTest'.
	self assert: stream contents equals: (('this is a test' copyWith: Character cr), 'for WriteStreamTest').
	
	"Manually put a new line and verify there are no 2 new lines"
	stream := self newStream.
	stream nextPutAll: ('this is a test' copyWith: Character cr).
	stream ensureEndsWith: Character cr.
	stream nextPutAll: 'for WriteStreamTest'.
	self assert: stream contents equals: (('this is a test' copyWith: Character cr), 'for WriteStreamTest').
	
	"Test with a empty stream"
	stream := self newStream.
	stream ensureEndsWith: Character cr.
	self assert: stream contents equals: ''.
]

{ #category : #tests }
WriteStreamTest >> testEnsureNoSpace [
	 
	| stream |
	stream := self newStream.
	stream ensureNoSpace.
	
	self assert: stream contents equals: ''.
	
	stream nextPutAll: ' a '.
	stream ensureNoSpace.
	self assert: stream contents equals: ' a'.
	
	stream nextPutAll: 'b  '.
	stream ensureNoSpace.
	self assert: stream contents equals: ' ab'.

	
	
	
]

{ #category : #tests }
WriteStreamTest >> testIsEmpty [
	| stream |
	stream := self newStream.
	self assert: stream isEmpty.
	stream nextPut: $a.
	self deny: stream isEmpty.
	stream reset.
	self deny: stream isEmpty.
]

{ #category : #tests }
WriteStreamTest >> testLessThanLessThan [
	"Tests for #<< output operator shortcut on WriteStreams"
	
	self 
		assert: (String streamContents: [ :out | out << $a << 'bcd' << $e ])
		equals: 'abcde'.
	self 
		assert: (ByteArray streamContents: [ :out | out << 1 << #[2 3 4] << 5 ])
		equals: #[1 2 3 4 5].
	self 
		assert: (Array streamContents: [ :out | out << 1 << #(2 3 4) asOrderedCollection << 5 ])
		equals: #(1 2 3 4 5).
	"no conversions, wrong element type"
	self should: [ String streamContents: [ :out | out << 1234 ] ] raise: Error.
	self should: [ ByteArray streamContents: [ :out | out << 1234 ] ] raise: Error
]

{ #category : #tests }
WriteStreamTest >> testNew [

	self should: [self streamClass new] raise: Error. 
]

{ #category : #tests }
WriteStreamTest >> testNextPut [
	| stream |
	stream := self newStream.
	self string
		withIndexDo: [ :each :i | 
			self
				assert: (stream nextPut: each) equals: each;
				assert: stream position equals: i;
				assert: stream contents equals: (self string first: i) ]
]

{ #category : #tests }
WriteStreamTest >> testNextPutAll [
	| stream |
	stream := self newStream.
	self
		assert: (stream nextPutAll: self string) equals: self string;
		assert: stream position equals: self stringSize;
		assert: stream contents equals: self string
]

{ #category : #tests }
WriteStreamTest >> testOn [
	| collection stream |
	collection := String new.
	stream := self streamClass on: collection.
	self
		assert: stream originalContents identicalTo: collection;
		assert: stream position isZero;
		assert: stream contents isEmpty;
		assert: stream size isZero
]

{ #category : #tests }
WriteStreamTest >> testOnFromTo [
	| stream |
	stream := self streamClass on: self string , self string from: self stringSize + 1 to: self stringSize * 2.
	self
		assert: stream originalContents equals: self string , self string;
		assert: stream position equals: self stringSize;
		assert: stream contents equals: self string;
		assert: stream size equals: self stringSize * 2
]

{ #category : #tests }
WriteStreamTest >> testPosition [
	| stream |
	stream := self newStream.
	stream nextPutAll: self string.
	0 to: self stringSize do: [ :i | 
		self
			assert: stream position equals: self stringSize;
			assert: (stream position: i) identicalTo: stream;
			assert: stream position equals: i;
			assert: stream contents equals: (self string first: i).
		stream position: self stringSize ].
	self should: [ stream position: -1 ] raise: Error.
	self should: [ stream position: self stringSize + 1 ] raise: Error
]

{ #category : #tests }
WriteStreamTest >> testSetToEnd [
	| stream |
	stream := self newStream.
	self
		assert: stream setToEnd identicalTo: stream;
		assert: stream position isZero;
		assert: stream contents isEmpty.
	stream nextPutAll: self string.
	self stringSize - 1 to: 0 by: -1 do: [ :i | 
		stream position: i.
		self
			assert: stream setToEnd identicalTo: stream;
			assert: stream position equals: self stringSize;
			assert: stream contents equals: self string ]
]

{ #category : #tests }
WriteStreamTest >> testWith [
	"like on: except it starts writing at the end of its argument collection,
	and the initial position is the collection size and the initial contents
	is the collection"

	| stream |
	stream := self streamClass with: self string.
	self
		assert: stream originalContents equals: self string;
		assert: stream position equals: self stringSize;
		assert: stream contents equals: self string;
		assert: stream size equals: self stringSize
]

{ #category : #tests }
WriteStreamTest >> testWithFromTo [
	"like with: except it paritions its argument collection first using the
	from:/to: indexes"

	| stream |
	stream := self streamClass with: self string , self string from: 1 to: self stringSize.
	self
		assert: stream originalContents equals: self string;
		assert: stream position equals: self stringSize;
		assert: stream contents equals: self string;
		assert: stream size equals: self stringSize
]
