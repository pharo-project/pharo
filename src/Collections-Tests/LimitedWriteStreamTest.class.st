"
SUnit test clss for LimitedWriteStream
"
Class {
	#name : #LimitedWriteStreamTest,
	#superclass : #WriteStreamTest,
	#category : #'Collections-Tests-Streams'
}

{ #category : #accessing }
LimitedWriteStreamTest >> streamClass [
	^ LimitedWriteStream
]

{ #category : #tests }
LimitedWriteStreamTest >> testLimit [
	| stream overLimit |
	stream := self newStream.
	overLimit := false.
	stream
		limitBlock: [ overLimit := true ];
		nextPutAll: self string.
	self
		assert: stream limit equals: self streamClass defaultLimit;
		assert: (stream limit: self stringSize) identicalTo: stream;
		assert: stream limit equals: self stringSize;
		deny: overLimit.

	self stringSize - 1 to: 0 by: -1 do: [ :i | 
		overLimit := false.
		self
			assert: (stream limit: i) identicalTo: stream;
			assert: stream limit equals: i;
			assert: stream position equals: i;
			assert: stream contents equals: (self string first: i);
			assert: overLimit ]
]

{ #category : #tests }
LimitedWriteStreamTest >> testLimitBlock [
	| stream overLimit limitBlock |
	stream := self newStream.
	overLimit := false.
	limitBlock := [ overLimit := true ].
	self
		assert: stream limitBlock isNil;
		assert: (stream limitBlock: limitBlock) identicalTo: stream;
		assert: stream limitBlock identicalTo: limitBlock;
		deny: overLimit
]

{ #category : #tests }
LimitedWriteStreamTest >> testNextPut [
	| stream overLimit |
	super testNextPut.

	stream := self streamClass on: String new limit: self stringSize limitBlock: [ overLimit := true ].
	overLimit := false.
	self string
		withIndexDo: [ :each :i | 
			self
				assert: (stream nextPut: each) equals: each;
				assert: stream position equals: i;
				assert: stream contents equals: (self string first: i);
				deny: overLimit ].
	self string
		do: [ :each | 
			overLimit := false.
			self
				assert: (stream nextPut: each) equals: each;
				assert: stream position equals: self stringSize;
				assert: stream contents equals: self string;
				assert: overLimit ]
]

{ #category : #tests }
LimitedWriteStreamTest >> testNextPutAll [
	| stream overLimit |
	super testNextPutAll.
	stream := self streamClass on: String new limit: self stringSize limitBlock: [ overLimit := true ].
	overLimit := false.
	self
		assert: (stream nextPutAll: self string) equals: self string;
		assert: stream position equals: self stringSize;
		assert: stream contents equals: self string;
		deny: overLimit.
	self
		assert: (stream nextPutAll: self string) equals: self string;
		assert: stream position equals: self stringSize;
		assert: stream contents equals: self string;
		assert: overLimit
]

{ #category : #tests }
LimitedWriteStreamTest >> testOn [
	| stream |
	super testOn.
	stream := self newStream.
	self
		assert: stream limit equals: self streamClass defaultLimit;
		assert: stream limitBlock isNil
]

{ #category : #tests }
LimitedWriteStreamTest >> testOnFromTo [
	| stream |
	super testOnFromTo.
	stream := self streamClass on: self string , self string from: self stringSize + 1 to: self stringSize * 2.
	self
		assert: stream limit equals: self streamClass defaultLimit;
		assert: stream limitBlock isNil
]

{ #category : #tests }
LimitedWriteStreamTest >> testOnLimitLimitBlock [
	| collection overLimit limitBlock stream |
	collection := String new.
	overLimit := false.
	limitBlock := [ overLimit := true ].
	stream := self streamClass on: collection limit: self stringSize limitBlock: limitBlock.
	self
		assert: stream originalContents identicalTo: collection;
		assert: stream position isZero;
		assert: stream limit equals: self stringSize;
		assert: stream limitBlock equals: limitBlock;
		deny: overLimit
]

{ #category : #tests }
LimitedWriteStreamTest >> testSetLimitLimitBlock [
	| stream overLimit |
	stream := self newStream.
	overLimit := false.
	self assert: (stream setLimit: self stringSize limitBlock: [ overLimit := true ]) identicalTo: stream.
	stream nextPutAll: self string.
	self deny: overLimit.

	self stringSize - 1 to: 0 by: -1 do: [ :i | 
		overLimit := false.
		"ensure the new block argument is used when the new limit is less than
		the position by setting it to nil first"
		self assert: (stream setLimit: stream position limitBlock: nil) identicalTo: stream.
		self deny: overLimit.
		self assert: (stream setLimit: i limitBlock: [ overLimit := true ]) identicalTo: stream.
		self assert: overLimit ]
]

{ #category : #tests }
LimitedWriteStreamTest >> testWith [
	"like on: except it starts writing at the end of its argument collection,
	and the initial position is the collection size and the initial contents
	is the collection"

	| stream overLimit |
	super testWith.

	stream := self streamClass with: self string.
	overLimit := false.
	stream
		limit: self stringSize * 2;
		limitBlock: [ overLimit := true ].
	self string
		withIndexDo: [ :each :i | 
			self
				assert: (stream nextPut: each) equals: each;
				assert: stream position equals: self stringSize + i;
				assert: stream contents equals: self string , (self string first: i);
				deny: overLimit ].
	self string
		do: [ :each | 
			overLimit := false.
			self
				assert: (stream nextPut: each) equals: each;
				assert: stream position equals: self stringSize * 2;
				assert: stream contents equals: self string , self string;
				assert: overLimit ]
]

{ #category : #tests }
LimitedWriteStreamTest >> testWithFromTo [
	"like with: except it paritions its argument collection first using the
	from:/to: indexes"

	| stream overLimit |
	super testWithFromTo.

	stream := self streamClass with: self string , self string from: 1 to: self stringSize.
	overLimit := false.
	stream
		limit: self stringSize * 2;
		limitBlock: [ overLimit := true ].
	self string
		withIndexDo: [ :each :i | 
			self
				assert: (stream nextPut: each) equals: each;
				assert: stream position equals: self stringSize + i;
				assert: stream contents equals: self string , (self string first: i);
				deny: overLimit ].
	self string
		do: [ :each | 
			overLimit := false.
			self
				assert: (stream nextPut: each) equals: each;
				assert: stream position equals: self stringSize * 2;
				assert: stream contents equals: self string , self string;
				assert: overLimit ]
]
