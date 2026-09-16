"
A NewBinaryFileStreamTest is a test class for testing the behavior of NewBinaryFileStream
"
Class {
	#name : 'NewBinaryFileStreamTest',
	#superclass : 'TestCase',
	#category : 'NewFiles-Tests',
	#package : 'NewFiles-Tests'
}

{ #category : 'private' }
NewBinaryFileStreamTest >> fileStreamForFileNamed: fileName [
	^ (FileAPI openAlways: fileName) binaryStream
]

{ #category : 'private' }
NewBinaryFileStreamTest >> killTestFile [
	(FileAPI exists: 'testFile') ifTrue: [
		FileAPI deleteFile: 'testFile'
	].

]

{ #category : 'running' }
NewBinaryFileStreamTest >> setUp [
	self killTestFile.
	
]

{ #category : 'running' }
NewBinaryFileStreamTest >> tearDown [ 
	self killTestFile.
	
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testEmptyFileIsAtEnd [
	| file |
	file := self fileStreamForFileNamed: 'testFile'.
	self assert: file atEnd.
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testFileWithSomeBytesSizeIsNotZero [

	| file |
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	self assert: file position equals: 1.
	file nextPutAll: #[1 2 3].
	self assert: file position equals: 4.
	file nextPut: 1.
	self assert: file position equals: 5.
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testFullFileIsAtEnd [

	| file |
	
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	self assert: file atEnd.
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testOpenFile [

	| file |
	
	file := self fileStreamForFileNamed: 'testFile'.
	file close.

	self assert: (FileAPI exists: 'testFile')
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testPeekDoesNotAdvanceTheStream [

	| file |
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	file nextPut: 2.
	file nextPut: 3.
	file close.

	file := self fileStreamForFileNamed: 'testFile'.
	self assert: file position equals: 0.
	self assert: file peek equals: file peek.
	self assert: file position equals: 0.
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testReadFullFileIsAtEnd [

	| file |
	
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	file close.

	file := self fileStreamForFileNamed: 'testFile'.
	file next.
	self assert: file atEnd.
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testReadLessThanAvailableYieldsJustRead [

	| file |
	
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	file nextPut: 2.
	file nextPut: 3.
	file close.

	file := self fileStreamForFileNamed: 'testFile'.
	self assert: (file next:2) equals: #[1 2].
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testReadMoreThanAvailableYieldsOnlyAvailable [

	| file |
	
	file := self fileStreamForFileNamed: 'testFile'.
	self assert: (file next:2) equals: #[].

	"then we put one element and we close it"
	file nextPut: 1.
	file close.

	file := self fileStreamForFileNamed: 'testFile'.
	self assert: (file next:2) equals: #[1].
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testReadMultipleBytes [

	| file |
	
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	file nextPut: 2.
	file close.

	file := self fileStreamForFileNamed: 'testFile'.
	self assert: (file next: 2) equals: #[1 2].
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testReadWhenNothingAvailableYieldsNil [

	| file |
	
	file := self fileStreamForFileNamed: 'testFile'.
	self assert: file next equals: nil.
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testSkipLecture [

	| file |
	
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	file nextPut: 2.
	file nextPut: 3.
	file close.

	file := self fileStreamForFileNamed: 'testFile'.
	file skip: 2.
	self assert: file next equals: 3.
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testWriteFromStartingAtCount [

	| file |
	
	file := self fileStreamForFileNamed: 'testFile'.
	file writeFrom: #[1 2 3] startingAt: 2 count: 2.
	file close.

	file := self fileStreamForFileNamed: 'testFile'.
	self assert: file next equals: 2.
	self assert: file next equals: 3.
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testWriteMultipleBytes [

	| file |
	
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPutAll: #[1 2].
	file close.

	file := self fileStreamForFileNamed: 'testFile'.
	self assert: file next equals: 1.
	self assert: file next equals: 2.
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testWriteReadInt [

	| file |
	
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	file close.

	file := self fileStreamForFileNamed: 'testFile'.
	self assert: file next equals: 1.
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testWriteReadInt2 [

	| file |
	
	file := self fileStreamForFileNamed: 'testFile'.
	file nextPut: 1.
	file position: 0.
	self assert: file next equals: 1.
	file close.
]

{ #category : 'tests' }
NewBinaryFileStreamTest >> testWriteToClosedFileFails [

	| fileStream |
	
	fileStream := self fileStreamForFileNamed: 'testFile'.
	fileStream close.
	self should: [ fileStream wrappedStream nextPut: 1 ] raise: Error
]
