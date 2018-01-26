Class {
	#name : #CompressedSegmentsStreamTest,
	#superclass : #TestCase,
	#category : #'CompressedSegmentsStream-Tests'
}

{ #category : #tests }
CompressedSegmentsStreamTest >> maxSize [

	^ 500
]

{ #category : #tests }
CompressedSegmentsStreamTest >> segmentSize [

	^ 100
]

{ #category : #tests }
CompressedSegmentsStreamTest >> testAtEnd [

	| data |
	
	data := #[1 2 3].

	self withEmptyStreamDo: [ :compressed |
		compressed nextPutAll: data.
		self assert: compressed atEnd.
		compressed position: 1.
		self deny: compressed atEnd.]
]

{ #category : #tests }
CompressedSegmentsStreamTest >> testEmpty [

	self withEmptyStreamDo: [ :compressed |
		self assert: compressed size equals: 0. 
		self assert: compressed reset contents isEmpty. ]
]

{ #category : #tests }
CompressedSegmentsStreamTest >> testNextPut [

	| data |
	
	data := 42.

	self withEmptyStreamDo: [ :compressed |
		compressed nextPut: data.
		self assert: compressed size equals: 1.
		self assert: compressed reset next equals: data. ]
]

{ #category : #tests }
CompressedSegmentsStreamTest >> testNextPutAll [

	| data |
	
	data := #[1 2 3].

	self withEmptyStreamDo: [ :compressed |
		compressed nextPutAll: data.
		self assert: compressed size equals: data size.
		self assertCollection: compressed contentsOfEntireFile equals: data. ]
]

{ #category : #tests }
CompressedSegmentsStreamTest >> testNextPutAllSegmentSize [

	| data |
	
	data := (1 to: self segmentSize) asByteArray.

	self withEmptyStreamDo: [ :compressed |
		compressed nextPutAll: data.
		self assert: compressed size equals: data size.
		self assertCollection: compressed contentsOfEntireFile equals: data. ]
]

{ #category : #tests }
CompressedSegmentsStreamTest >> testNextPutAllSegmentSizePlusOne [

	| data |
	
	data := (1 to: self segmentSize + 1) asByteArray.

	self withEmptyStreamDo: [ :compressed |
		compressed nextPutAll: data.
		self assert: compressed size equals: data size.
		self assertCollection: compressed contentsOfEntireFile equals: data. ]
]

{ #category : #tests }
CompressedSegmentsStreamTest >> testNextPutTwoAndHalfSegments [

	| data |
	
	data := (1 to: self segmentSize * 2 + (self segmentSize // 2)) asByteArray.

	self withEmptyStreamDo: [ :compressed |
		compressed nextPutAll: data.
		self assert: compressed size equals: data size.
		self assertCollection: compressed contentsOfEntireFile equals: data. ]
]

{ #category : #tests }
CompressedSegmentsStreamTest >> testNextPutTwoSegments [

	| data |
	
	data := (1 to: self segmentSize * 2) asByteArray.

	self withEmptyStreamDo: [ :compressed |
		compressed nextPutAll: data.
		self assert: compressed size equals: data size.
		self assertCollection: compressed contentsOfEntireFile equals: data. ]
]

{ #category : #tests }
CompressedSegmentsStreamTest >> testStoreAndReadOn: aFileReference [

	| fileName data compressedFile |

	data := (1 to: self segmentSize * 2 + (self segmentSize // 2)) asByteArray.

	fileName := 'testStoreAndRead.stc'.

	[
		aFileReference ensureDelete.
		aFileReference writeStreamDo: [ :stream | 
			| compressed |
			compressed := (CompressedSegmentsStream on: stream binary) segmentSize: self segmentSize maxSize: self maxSize.
			[ 
			compressed nextPutAll: data.
			] ensure: [compressed close]. ].

		compressedFile := CompressedSegmentsStream on: aFileReference readStream binary.
		self assertCollection: compressedFile contentsOfEntireFile equals: data.
		
	] ensure: [
		aFileReference ensureDelete.
	]

]

{ #category : #tests }
CompressedSegmentsStreamTest >> testStoreAndReadOnDisk [

	self testStoreAndReadOn: FileSystem disk workingDirectory / 'CompressedSegmentsStreamTests-testStoreAndRead.stc'.

]

{ #category : #tests }
CompressedSegmentsStreamTest >> testStoreAndReadOnMemory [

	self testStoreAndReadOn: FileSystem memory root / 'CompressedSegmentsStreamTests-testStoreAndRead.stc'.
]

{ #category : #tests }
CompressedSegmentsStreamTest >> withEmptyStreamDo: aBlock [
	| binary compressed |
	
	binary := ReadWriteStream on: ByteArray new.
	compressed := (CompressedSegmentsStream on: binary) segmentSize: self segmentSize maxSize: self maxSize.
	[ 
		aBlock value: compressed 
	] ensure: [ 
		compressed close.
		binary close. ]
]
