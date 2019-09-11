Class {
	#name : #ZnPositionableReadStreamTest,
	#superclass : #TestCase,
	#category : #'Zinc-Character-Encoding-Tests'
}

{ #category : #testing }
ZnPositionableReadStreamTest >> testBulkReading [
	| stream buffer |
	stream := ZnPositionableReadStream on: #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) readStream.
	buffer := Array new: 6.
	self assert: (stream readInto: buffer startingAt: 1 count: 6) equals: 6.
	self assert: buffer equals: #(0 1 2 3 4 5).
	self assert: (stream readInto: buffer startingAt: 4 count: 3) equals: 3.
	self assert: (stream readInto: buffer startingAt: 1 count: 3) equals: 3.
	self assert: buffer equals: #(9 10 11 6 7 8).
	buffer atAllPut: 0.
	self assert: (stream readInto: buffer startingAt: 1 count: 6) equals: 4.
	self assert: buffer equals: #(12 13 14 15 0 0)
]

{ #category : #testing }
ZnPositionableReadStreamTest >> testEmpty [
	| stream |
	stream := ZnPositionableReadStream on: '' readStream.
	self assert: stream atEnd.
	self assert: stream peek isNil.
	self assert: stream next isNil.
	self assert: stream position isZero

]

{ #category : #testing }
ZnPositionableReadStreamTest >> testNestedExcursion [
	| stream |
	stream := ZnPositionableReadStream on: 'abcdef---XYZ!1--------' readStream.
	self assert: (stream next: 3) equals: 'abc'.
	stream savingPositionDo: [ 
		self assert: (stream next: 3) equals: 'def'.
		stream savingPositionDo: [ 
			stream upTo: $!.
			self assert: (stream peekFor: $1) ].
		stream skip: 3.
		self assert: (stream next: 3) equals: 'XYZ' ].
	self assert: (stream next: 3) equals: 'def'

]

{ #category : #testing }
ZnPositionableReadStreamTest >> testNew [
	| stream |
	stream := ZnPositionableReadStream on: 'abc' readStream.
	self assert: stream position isZero.
	self assert: stream bufferSize equals: stream defaultBufferSize.
	self deny: stream atEnd


]

{ #category : #testing }
ZnPositionableReadStreamTest >> testPlainExcursion [
	| stream |
	stream := ZnPositionableReadStream on: 'abcdef-----------' readStream.
	self assert: (stream next: 3) equals: 'abc'.
	self assert: (stream savingPositionDo: [ stream next: 3 ]) equals: 'def'.
	self assert: (stream next: 3) equals: 'def'

]

{ #category : #testing }
ZnPositionableReadStreamTest >> testPlainNext [
	| stream |
	stream := ZnPositionableReadStream on: 'abc' readStream.
	self assert: stream next equals: $a.
	self deny: stream atEnd.
	self assert: stream next equals: $b.
	self assert: stream position equals: 2.
	self assert: stream next equals: $c.
	self assert: stream atEnd


]

{ #category : #testing }
ZnPositionableReadStreamTest >> testPlainPeek [
	| stream |
	stream := ZnPositionableReadStream on: 'abc' readStream.
	self assert: stream next equals: $a.
	self assert: stream peek equals: $b.
	self assert: stream next equals: $b.
	self assert: stream position equals: 2.
	self assert: stream peek equals: $c.
	self assert: stream next equals: $c.
	self assert: stream atEnd


]

{ #category : #testing }
ZnPositionableReadStreamTest >> testPositionErrors [
	| data stream |
	data := ByteArray new: 1000 streamContents: [ :out | 
		100 timesRepeat: [ out nextPutAll: #[ 0 1 2 3 4 5 6 7 8 9 ] ] ].
	stream := ZnPositionableReadStream on: data readStream.
	self should: [ stream position: 1 ] raise: SubscriptOutOfBounds.
	stream next: 100.
	self should: [ stream position: -1 ] raise: SubscriptOutOfBounds.
	self should: [ stream position: 101 ] raise: SubscriptOutOfBounds.
	stream next: 500.
	self should: [ stream position: 100 ] raise: SubscriptOutOfBounds.
	self should: [ stream position: 600 - stream bufferSize - 1 ] raise: SubscriptOutOfBounds.
	stream position: 400.
	stream next: 599.
	self assert: stream next equals: 9.
	self assert: stream atEnd
]

{ #category : #testing }
ZnPositionableReadStreamTest >> testReadAll [
	| data stream |
	data := String new: 200 streamContents: [ :out | 
		200 timesRepeat: [ out nextPut: 'abc' atRandom ] ].
	stream := ZnPositionableReadStream on: data readStream.
	self deny: stream atEnd.
	self assert: stream position isZero.
	stream savingPositionDo: [
		self assert: stream upToEnd equals: data.
		self assert: stream atEnd.
		self assert: stream position equals: 200 ].
	self deny: stream atEnd.
	self assert: stream position isZero.
	self assert: stream upToEnd equals: data.
	self assert: stream atEnd.
	self assert: stream position equals: 200
]

{ #category : #testing }
ZnPositionableReadStreamTest >> testReadAllLargerBuffer [
	| data stream |
	data := String new: 500 streamContents: [ :out | 
		500 timesRepeat: [ out nextPut: 'abc' atRandom ] ].
	stream := ZnPositionableReadStream on: data readStream.
	stream sizeBuffer: 500.
	self deny: stream atEnd.
	self assert: stream position isZero.
	stream savingPositionDo: [
		self assert: stream upToEnd equals: data.
		self assert: stream atEnd.
		self assert: stream position equals: 500 ].
	self deny: stream atEnd.
	self assert: stream position isZero.
	self assert: stream upToEnd equals: data.
	self assert: stream atEnd.
	self assert: stream position equals: 500
]

{ #category : #testing }
ZnPositionableReadStreamTest >> testSearch [
	| data stream found |
	data := String new: 2000 streamContents: [ :out | 
		2000 timesRepeat: [ out nextPut: 'abc' atRandom ] ].
	data replaceFrom: 1000 to: 1005 with: 'TARGET'.
	stream := ZnPositionableReadStream on: data readStream.
	found := false.
	[ stream atEnd ] whileFalse: [ 
		stream savingPositionDo: [ 
			(stream next: 6) = 'TARGET' 
				ifTrue: [ 
					found := true.
					self assert: stream position equals: 1005 ] ].
		stream next ].
	self assert: found
]

{ #category : #testing }
ZnPositionableReadStreamTest >> testSearchBinary [
	| data stream pattern found |
	data := ByteArray new: 2000 streamContents: [ :out | 
		2000 timesRepeat: [ out nextPut: 256 atRandom - 1 ] ].
	pattern := ByteArray readHexFrom: 'FF77ABAB'.
	data replaceFrom: 1000 to: 1000 + pattern size - 1 with: pattern.
	stream := ZnPositionableReadStream on: data readStream.
	found := false.
	[ stream atEnd ] whileFalse: [ 
		stream savingPositionDo: [ 
			(stream next: pattern size) = pattern 
				ifTrue: [ 
					found := true.
					self assert: stream position equals: 1000 + pattern size - 1 ] ].
		stream next ].
	self assert: found
]

{ #category : #testing }
ZnPositionableReadStreamTest >> testSkipAndBack [
	| stream |
	stream := ZnPositionableReadStream on: 'abcdef' readStream.
	stream skip: 2.
	self assert: stream next equals: $c.
	stream skip: 1.
	self assert: stream back equals: $d.
	self assert: stream back equals: $c.
	stream skip: -2.
	self assert: stream next equals: $a.
	stream back.
	self assert: stream upToEnd equals: 'abcdef'. 

]

{ #category : #testing }
ZnPositionableReadStreamTest >> testUTF8 [
	| data stream |
	data := 'Les élèves Françaises ont 100 €'.
	stream := ZnPositionableReadStream on: (ZnCharacterReadStream on: data utf8Encoded readStream).
	self assert: (stream next: 3) equals: 'Les'.
	stream skip: 1.
	stream savingPositionDo: [ 
		self assert: (stream next: 6) equals: 'élèves'.
		self assert: stream next equals: Character space ].
	self assert: (stream next: 6) equals: 'élèves'.
	self assert: (stream peekFor: Character space).
	2 timesRepeat: [ stream upTo: Character space ].
	self assert: (stream upTo: $€) trimBoth asNumber equals: 100.
	self assert: stream atEnd
]
