"
I represent a memory file system entry for a regular file
"
Class {
	#name : #MemoryFileSystemFile,
	#superclass : #MemoryFileSystemEntry,
	#instVars : [
		'bytes',
		'size'
	],
	#category : #'FileSystem-Memory-Base'
}

{ #category : #'stream-protocol' }
MemoryFileSystemFile >> at: index [
	self flag: #todo. "out of bounds checks?"
	^ bytes at: index
]

{ #category : #'stream-protocol' }
MemoryFileSystemFile >> at: index put: anObject [
	index > bytes size
		 ifTrue: [ self grow ].
		
	bytes at: index put: (anObject isCharacter
		ifTrue: [ anObject codePoint ]
		ifFalse: [ anObject ]).
	
	size := size max: index
]

{ #category : #'stream-protocol' }
MemoryFileSystemFile >> at: index read: aCollection startingAt: start count: count [ 
	| max stop |
	max := size - index + 1 min: count.
	stop := start + max - 1.
	aCollection 
		replaceFrom: start
		to: stop
		with: bytes
		startingAt: index.
	^ stop - start + 1
]

{ #category : #'stream-protocol' }
MemoryFileSystemFile >> at: first write: aCollection startingAt: start count: count [ 
	| last |
	last := first + count - 1.
	last > bytes size ifTrue: [ self grownBy: last - size ].
	bytes 
		replaceFrom: first
		to: last
		with: aCollection
		startingAt: start.
	size := last
]

{ #category : #'streams-compatibility' }
MemoryFileSystemFile >> binaryReadStream [
	^ ReadStream on: self bytes from: 1 to: size
]

{ #category : #streams }
MemoryFileSystemFile >> binaryWriteStream [

	^ MemoryFileWriteStream on: self
]

{ #category : #accessing }
MemoryFileSystemFile >> bytes [
	^ bytes
]

{ #category : #'stream-protocol' }
MemoryFileSystemFile >> copyFrom: from to: position [
	^ bytes copyFrom: from to: position
]

{ #category : #accessing }
MemoryFileSystemFile >> fileSize [
	^ size
]

{ #category : #'stream-protocol' }
MemoryFileSystemFile >> grow [
	self grownBy: self sizeIncrement
]

{ #category : #accessing }
MemoryFileSystemFile >> grownBy: length [
	bytes := bytes grownBy: length.
	
	self modified.
]

{ #category : #initialization }
MemoryFileSystemFile >> initialize [
	super initialize.
	bytes := #[].
	size := 0
]

{ #category : #accessing }
MemoryFileSystemFile >> internalSize [
	^ bytes size
]

{ #category : #testing }
MemoryFileSystemFile >> isDirectory [
	^ false
]

{ #category : #'stream-protocol' }
MemoryFileSystemFile >> readStream [
	^ ReadStream on: self bytes asString from: 1 to: size
]

{ #category : #accessing }
MemoryFileSystemFile >> size [

	^ size
]

{ #category : #accessing }
MemoryFileSystemFile >> sizeIncrement [
	^ (bytes size min: 20) max: 1024
]

{ #category : #accessing }
MemoryFileSystemFile >> truncate [
	self truncateTo: size
]

{ #category : #accessing }
MemoryFileSystemFile >> truncateTo: aSize [
	bytes size = aSize
		ifFalse: [bytes := bytes size < aSize
			ifTrue: [(ByteArray new: aSize)
				replaceFrom: 1 to: bytes size with: bytes startingAt: 1]
			ifFalse: [bytes copyFrom: 1 to: aSize]].
	size := bytes size.
	self modified.
]

{ #category : #private }
MemoryFileSystemFile >> updateContents: aCollection [ 
	
	bytes := aCollection.
	self updateSize: aCollection size.
]

{ #category : #private }
MemoryFileSystemFile >> updateSize: newSize [
	size := newSize.
	modificationTime := nil
]

{ #category : #'stream-protocol' }
MemoryFileSystemFile >> writeStreamDo: aBlock [
	| stream |
	stream := WriteStream on: ByteArray new.
	[ aBlock value: stream ] ensure: [ stream close ].
	bytes := stream contents.
	self updateSize: bytes size
]
