Class {
	#name : #SourceFileBufferedReadWriteStreamTest,
	#superclass : #TestCase,
	#category : #Tests
}

{ #category : #tests }
SourceFileBufferedReadWriteStreamTest >> testBuffering [
	| stream |
	stream := SourceFileBufferedReadWriteStream on: '01234567890123456789' readStream.
	stream sizeBuffer: 8.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self deny: stream atEnd.
	self assert: (stream next: 10) equals: '0123456789'.
	self assert: stream atEnd	
]

{ #category : #tests }
SourceFileBufferedReadWriteStreamTest >> testNextPutAllStartingAt [
	| stringStream |

	stringStream := ReadWriteStream with: ''.
	
	SourceFileBufferedReadWriteStream on: stringStream do: [ : bufferedStream |
		bufferedStream sizeBuffer: 8.
		bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
		bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6. 
		bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
		bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6.] .
	
	self assert: stringStream contents equals: '01234567890123456789'
]

{ #category : #tests }
SourceFileBufferedReadWriteStreamTest >> testPeek [
	| stream |
	stream := SourceFileBufferedReadWriteStream on: '0123456789' readStream.
	stream sizeBuffer: 8.
	'0123456789' do: [ :each |
		self deny: stream atEnd.
		self assert: stream peek equals: each.
		self assert: stream next equals: each ].
	self assert: stream atEnd.
	self assert: stream peek isNil.
	self assert: stream next isNil
]

{ #category : #tests }
SourceFileBufferedReadWriteStreamTest >> testReadInto [
	| stream buffer count |
	stream := SourceFileBufferedReadWriteStream on: '0123456789' readStream.
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

{ #category : #tests }
SourceFileBufferedReadWriteStreamTest >> testReadIntoLarger [
	| stream buffer count |
	stream := SourceFileBufferedReadWriteStream on: '0123456789' readStream.
	stream sizeBuffer: 4.
	buffer := String new: 10.
	count := stream readInto: buffer startingAt: 1 count: 10.
	self assert: count equals: 10.
	self assert: buffer equals: '0123456789'	 
]

{ #category : #tests }
SourceFileBufferedReadWriteStreamTest >> testReadThenWrite [
	| stream stringStream |
	
	((SystemVersion current major < 7) or: [ SystemVersion current build < 690 ])
		ifTrue: [ ^ self skip ].

	stringStream := ReadWriteStream with: '0123456789'.
	stringStream reset.
	stream := SourceFileBufferedReadWriteStream on: stringStream.
	stream sizeBuffer: 8.
	
	stream next: 4.
	self assert: stream position equals: 4.
	
	stream nextPutAll: 'ABCD'.
	self assert: stream position equals: 8.
		
	self assert: stream peek equals: $8. 
	self assert: stream upToEnd equals: '89'.
	self assert: stream atEnd.
	
	self assert: stringStream contents equals: '0123ABCD89'
]

{ #category : #tests }
SourceFileBufferedReadWriteStreamTest >> testReadUpTo [
	| stream |
	stream := SourceFileBufferedReadWriteStream on: '0123456789' readStream.
	stream sizeBuffer: 8.
	self assert: (stream upTo: $5) equals: '01234'.
	self assert: stream upToEnd equals: '6789'.
	self assert: stream atEnd
]

{ #category : #tests }
SourceFileBufferedReadWriteStreamTest >> testReadUpToEnd [
	| stream |
	stream := SourceFileBufferedReadWriteStream on: '0123456789' readStream.
	stream sizeBuffer: 4.
	stream next: 2.
	self assert: stream upToEnd equals: '23456789'.
	self assert: stream atEnd
]

{ #category : #tests }
SourceFileBufferedReadWriteStreamTest >> testSetAtEnd [

	| stream originalStream|

	stream := SourceFileBufferedReadWriteStream on: (originalStream := ReadWriteStream with: '0123456789').
	originalStream reset.
	
	stream setToEnd.
	
	stream nextPut: $A.
	stream flush.

	self assert: originalStream contents equals: '0123456789A'.

]

{ #category : #tests }
SourceFileBufferedReadWriteStreamTest >> testWriteThenRead [
	| stream stringStream |
	
	((SystemVersion current major < 7) or: [ SystemVersion current build < 690 ])
		ifTrue: [ ^ self skip ].

	stringStream := ReadWriteStream with: '0123456789'.
	stringStream reset.
	stream := SourceFileBufferedReadWriteStream on: stringStream.
	stream sizeBuffer: 8.
	
	stream nextPutAll: 'ABCD'.
	
	self assert: stream peek equals: $4. 
	self assert: stream position equals: 4.
	self assert: stream upToEnd equals: '456789'.
	self assert: stream position equals: 10.
	self assert: stream atEnd.
	
	self assert: stringStream contents equals: 'ABCD456789'
]

{ #category : #tests }
SourceFileBufferedReadWriteStreamTest >> testWriting [
	| stringStream bufferedStream |
	stringStream := ReadWriteStream with: ''.

	bufferedStream := SourceFileBufferedReadWriteStream on: stringStream.
	0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
	bufferedStream flush.

	self assert: stringStream contents equals: '0123456789'
]

{ #category : #tests }
SourceFileBufferedReadWriteStreamTest >> testWritingOverflow [
	| stringStream bufferedStream |
	
	stringStream := ReadWriteStream with: ''.
	bufferedStream := SourceFileBufferedReadWriteStream on: stringStream.
	bufferedStream sizeBuffer: 8.
	0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
	bufferedStream nextPutAll: '0123'; nextPutAll: '4567'; nextPutAll: '89'.
	bufferedStream nextPutAll: '0123456789'; nextPutAll: '0123456789'.
	bufferedStream flush.
		
	self assert: stringStream contents equals: '0123456789012345678901234567890123456789'
]
