Class {
	#name : #ZnBufferedWriteStreamTest,
	#superclass : #TestCase,
	#category : #'Zinc-Character-Encoding-Tests'
}

{ #category : #testing }
ZnBufferedWriteStreamTest >> testNextPutAllStartingAt [
	| string |
	string := String streamContents: [ :stringStream | 
		ZnBufferedWriteStream on: stringStream do: [ : bufferedStream |
			bufferedStream sizeBuffer: 8.
			bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
			bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6. 
			bufferedStream next: 5 putAll: '--012345--' startingAt: 3.
			bufferedStream next: 5 putAll: '0123456789XX' startingAt: 6.] ].
	self assert: string equals: '01234567890123456789'
]

{ #category : #testing }
ZnBufferedWriteStreamTest >> testWriting [
	| string |
	string := String
		streamContents: [ :stringStream | 
			| bufferedStream |
			bufferedStream := ZnBufferedWriteStream on: stringStream.
			0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
			bufferedStream flush ].
	self assert: string equals: '0123456789'
]

{ #category : #testing }
ZnBufferedWriteStreamTest >> testWritingOverflow [
	| string |
	string := String
		streamContents: [ :stringStream | 
			| bufferedStream |
			bufferedStream := ZnBufferedWriteStream on: stringStream.
			bufferedStream sizeBuffer: 8.
			0 to: 9 do: [ :each | bufferedStream nextPut: (Character digitValue: each) ].
			bufferedStream
				nextPutAll: '0123';
				nextPutAll: '4567';
				nextPutAll: '89'.
			bufferedStream
				nextPutAll: '0123456789';
				nextPutAll: '0123456789'.
			bufferedStream flush ].
	self assert: string equals: '0123456789012345678901234567890123456789'
]
