"
I am ZnBufferedReadStreamTests.
"
Class {
	#name : 'ZnBufferedReadStreamTest',
	#superclass : 'TestCase',
	#category : 'Zinc-Character-Encoding-Tests',
	#package : 'Zinc-Character-Encoding-Tests'
}

{ #category : 'tests' }
ZnBufferedReadStreamTest >> testBack [
	"Check that ZnBufferedReadStream>>#back behaves as expected"
	<gtExample>
	| stream source |

	"Allocate a buffer larger than the default size (65536)"
	self assert: ZnBufferedReadStream basicNew defaultBufferSize < 66000.
	source := ByteArray new: 70000.
	1 to: 70000 do: [ :i |
		source at: i put: i \\ 256 ].

	stream := ZnBufferedReadStream on: source readStream.
	stream position: 1.
	self assert: stream peek equals: (source at: 2).
	self assert: stream back equals: (source at: 1).
	"Position the stream beyond the end of the initial buffer"
	stream position: 66000.
	self assert: stream peek equals: (source at: 66001).
	self assert: stream back equals: (source at: 66000).
]

{ #category : 'tests' }
ZnBufferedReadStreamTest >> testBuffering [
	| stream |
	stream := ZnBufferedReadStream on: '01234567890123456789' readStream.
	stream sizeBuffer: 8.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self assert: stream atEnd
]

{ #category : 'tests' }
ZnBufferedReadStreamTest >> testPeek [
	| stream |
	stream := ZnBufferedReadStream on: '0123456789' readStream.
	stream sizeBuffer: 8.
	'0123456789' do: [ :each |
		self deny: stream atEnd.
		self assert: stream peek equals: each.
		self assert: stream next equals: each ].
	self assert: stream atEnd.
	self assert: stream peek isNil.
	self assert: stream next isNil
]

{ #category : 'tests' }
ZnBufferedReadStreamTest >> testReadInto [
	| stream buffer count |
	stream := ZnBufferedReadStream on: '0123456789' readStream.
	stream sizeBuffer: 8.
	buffer := String new: 6 withAll: Character space.
	stream skip: 1.
	stream readInto: buffer startingAt: 4 count: 3.
	self assert: buffer equals: '   123'.
	stream readInto: buffer startingAt: 1 count: 3.
	self assert: buffer equals: '456123'.
	count := stream readInto: buffer startingAt: 1 count: 100.
	self assert: count equals: 3.
	self assert: buffer equals: '789123'
]

{ #category : 'tests' }
ZnBufferedReadStreamTest >> testReadIntoLarger [
	| stream buffer count |
	stream := ZnBufferedReadStream on: '0123456789' readStream.
	stream sizeBuffer: 4.
	buffer := String new: 10.
	count := stream readInto: buffer startingAt: 1 count: 10.
	self assert: count equals: 10.
	self assert: buffer equals: '0123456789'
]

{ #category : 'tests' }
ZnBufferedReadStreamTest >> testReadUpTo [
	| stream |
	stream := ZnBufferedReadStream on: '0123456789' readStream.
	stream sizeBuffer: 8.
	self assert: (stream upTo: $5) equals: '01234'.
	self assert: stream upToEnd equals: '6789'.
	self assert: stream atEnd
]

{ #category : 'tests' }
ZnBufferedReadStreamTest >> testReadUpToEnd [
	| stream |
	stream := ZnBufferedReadStream on: '0123456789' readStream.
	stream sizeBuffer: 4.
	stream next: 2.
	self assert: stream upToEnd equals: '23456789'.
	self assert: stream atEnd.
	stream := ZnBufferedReadStream on: #[] readStream.
	self assert: stream upToEnd equals: #[].
	self assert: stream atEnd
]

{ #category : 'tests' }
ZnBufferedReadStreamTest >> testSetToEnd [
	| stream source |
	source := '0123456789'.
	stream := ZnBufferedReadStream on: source readStream.
	stream sizeBuffer: source size.

	"Call setToEnd on new stream"
	self assert: stream position equals: 0.
	stream setToEnd.
	self assert: stream position equals: source size.

	"Call setToEnd without after reading some elements"
	stream position: 2.
	self assert: (stream next: 4) equals: '2345'.
	stream setToEnd.
	self assert: stream position equals: source size
]
