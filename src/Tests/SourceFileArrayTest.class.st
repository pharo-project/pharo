"
This test documents the source pointer address conversion methods for ExpandedSourceFileArray.

The available address space for source pointers in a traditional CompiledMethod is 16r1000000 through 16r4FFFFFF. StandardSourceFileArray maps positions in the sources file to address range 16r1000000 through 16r1FFFFFF and 16r3000000 through 16r3FFFFFF, and positions in the changes file to address range 16r2000000 through 16r2FFFFFF and 16r4000000 through 16r4FFFFFF. This permits a maximum file size of 16r2000000 (32MB) for both the sources file and the changes file. 

ExpandedSourceFileArray extends the source pointer address space using bit 25 of the source pointer to identify the external sources and changes files, with the remaining high order bits treated as address extension. This limits the number of external file references to two (the traditional sources and changes files). If additional external file references are needed in the future, some higher order bits in the source pointer address space should be allocated for that purpose.

The use of bit 25 of the source pointer for file references permits backward compatibility with StandardSourceFileArray, with essentially unlimited address space expansion for the sources and changes files.

"
Class {
	#name : #SourceFileArrayTest,
	#superclass : #TestCase,
	#category : #'Tests-Files'
}

{ #category : #running }
SourceFileArrayTest >> ensureChangesFileOpenedInProcess [
	"Ensure the changes file is open"

	| remoteString |
	remoteString := SourceFiles remoteStringAt: thisContext method sourcePointer.

	self deny: remoteString string isNil.
	"self deny: ProcessAndSessionLocalSourcesFileArray value isNil."
]

{ #category : #testing }
SourceFileArrayTest >> testAddressRange [
	"Test source pointer to file position address translation across a wide address range"

	| sf i p a |
	sf := SourceFileArray new.
	16r1000000 to: 16r10000000 by: 4093 do: [ :e | 
		i := sf fileIndexFromSourcePointer: e.
		p := sf filePositionFromSourcePointer: e.
		a := sf sourcePointerFromFileIndex: i andPosition: p.
		self assert: a equals: e ]
]

{ #category : #testing }
SourceFileArrayTest >> testChangesFileAddressRange [
	"Test file position to source pointer address translation for the changes file"

	| sf i p a a2 |
	sf := SourceFileArray new.
	0 to: 16r1FFFFFFF by: 4093 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		i := sf fileIndexFromSourcePointer: a.
		self assert: i equals: 2.
		p := sf filePositionFromSourcePointer: a.
		self assert: p equals: e.
		a2 := sf sourcePointerFromFileIndex: 2 andPosition: p.
		self assert: a2 equals: a ].
	0 to: 16rFFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r2000000 and: 16r2FFFFFF) ].
	16r1000000 to: 16r1FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r4000000 and: 16r4FFFFFF) ].
	16r2000000 to: 16r2FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r6000000 and: 16r6FFFFFF) ].
	16r3000000 to: 16r3FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r8000000 and: 16r8FFFFFF) ].
	16r4000000 to: 16r4FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16rA000000 and: 16rAFFFFFF) ].
	16r5000000 to: 16r5FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16rC000000 and: 16rCFFFFFF) ].
	16r6000000 to: 16r6FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16rE000000 and: 16rEFFFFFF) ].
	16r7000000 to: 16r7FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 2 andPosition: e.
		self assert: (a between: 16r10000000 and: 16r10FFFFFF) ]
]

{ #category : #testing }
SourceFileArrayTest >> testChangesFileStream [

	self assert: SourceFiles changesFileStream isNotNil
]

{ #category : #testing }
SourceFileArrayTest >> testFileIndexFromSourcePointer [
	"Test derivation of file index for sources or changes file from source pointers"

	| sf |
	sf := SourceFileArray new.
	"sources file mapping"
	self assert: 1 equals: (sf fileIndexFromSourcePointer: 16r1000000).
	self assert: 1 equals: (sf fileIndexFromSourcePointer: 16r1000013).
	self assert: 1 equals: (sf fileIndexFromSourcePointer: 16r1FFFFFF).
	self assert: 1 equals: (sf fileIndexFromSourcePointer: 16r3000000).
	self assert: 1 equals: (sf fileIndexFromSourcePointer: 16r3000013).
	self assert: 1 equals: (sf fileIndexFromSourcePointer: 16r3FFFFFF).

	self assert: 1 equals: (sf fileIndexFromSourcePointer: 16r5000000).
	self assert: 1 equals: (sf fileIndexFromSourcePointer: 16r5000013).
	self assert: 1 equals: (sf fileIndexFromSourcePointer: 16r5FFFFFF).

	(16r1000000 to: 16r1FFFFFF by: 811) do: [ :e | self assert: 1 equals: (sf fileIndexFromSourcePointer: e) ].
	(16r3000000 to: 16r3FFFFFF by: 811) do: [ :e | self assert: 1 equals: (sf fileIndexFromSourcePointer: e) ].
	"changes file mapping"
	self assert: 2 equals: (sf fileIndexFromSourcePointer: 16r2000000).
	self assert: 2 equals: (sf fileIndexFromSourcePointer: 16r2000013).
	self assert: 2 equals: (sf fileIndexFromSourcePointer: 16r2FFFFFF).
	self assert: 2 equals: (sf fileIndexFromSourcePointer: 16r4000000).
	self assert: 2 equals: (sf fileIndexFromSourcePointer: 16r4000013).
	self assert: 2 equals: (sf fileIndexFromSourcePointer: 16r4FFFFFF).

	self assert: 2 equals: (sf fileIndexFromSourcePointer: 16r6000000).
	self assert: 2 equals: (sf fileIndexFromSourcePointer: 16r6000013).
	self assert: 2 equals: (sf fileIndexFromSourcePointer: 16r6FFFFFF).

	(16r2000000 to: 16r2FFFFFF by: 811) do: [ :e | self assert: 2 equals: (sf fileIndexFromSourcePointer: e) ].
	(16r4000000 to: 16r4FFFFFF by: 811) do: [ :e | self assert: 2 equals: (sf fileIndexFromSourcePointer: e) ].

	"the following numeric ranges are unused but currently produces results as follows"
	self assert: 0 equals: (sf fileIndexFromSourcePointer: 16r0000000).
	self assert: 0 equals: (sf fileIndexFromSourcePointer: 16r0000013).
	self assert: 0 equals: (sf fileIndexFromSourcePointer: 16r0FFFFFF)
]

{ #category : #testing }
SourceFileArrayTest >> testFilePositionFromSourcePointer [
	"Test derivation of file position for sources or changes file from source pointers"

	| sf |
	sf := SourceFileArray new.
	"sources file"
	self assert: 0 equals: (sf filePositionFromSourcePointer: 16r1000000).
	self assert: 16r13 equals: (sf filePositionFromSourcePointer: 16r1000013).
	self assert: 16rFFFFFF equals: (sf filePositionFromSourcePointer: 16r1FFFFFF).
	self assert: 16r1000000 equals: (sf filePositionFromSourcePointer: 16r3000000).
	self assert: 16r1000013 equals: (sf filePositionFromSourcePointer: 16r3000013).
	self assert: 16r1FFFFFF equals: (sf filePositionFromSourcePointer: 16r3FFFFFF).
	"changes file"
	self assert: 0 equals: (sf filePositionFromSourcePointer: 16r2000000).
	self assert: 16r13 equals: (sf filePositionFromSourcePointer: 16r2000013).
	self assert: 16rFFFFFF equals: (sf filePositionFromSourcePointer: 16r2FFFFFF).
	self assert: 16r1000000 equals: (sf filePositionFromSourcePointer: 16r4000000).
	self assert: 16r1000013 equals: (sf filePositionFromSourcePointer: 16r4000013).
	self assert: 16r1FFFFFF equals: (sf filePositionFromSourcePointer: 16r4FFFFFF).
	"the following numeric ranges are unused but currently produces results as follows"
	self assert: 0 equals: (sf filePositionFromSourcePointer: 16r0000000).
	self assert: 16r13 equals: (sf filePositionFromSourcePointer: 16r0000013).
	self assert: 16rFFFFFF equals: (sf filePositionFromSourcePointer: 16r0FFFFFF)
]

{ #category : #testing }
SourceFileArrayTest >> testForkedRead [

	| originalString remoteString readSemaphore readString testSemaphore |
	originalString := '"test"'.
	remoteString := nil.
	readSemaphore := Semaphore new.
	testSemaphore := Semaphore new.

	self ensureChangesFileOpenedInProcess.
	
	[	self ensureChangesFileOpenedInProcess.

		readSemaphore wait.

		"Read the string that was written in other process."
		readString := remoteString string.
		testSemaphore signal 
		] fork.

	"Write the string, that will be read in other process."
	remoteString := SourceFiles remoteStringForNewString: originalString.

	readSemaphore signal.
	testSemaphore wait.
	
	self assert: readString equals: originalString.
]

{ #category : #testing }
SourceFileArrayTest >> testForkedWrite [

	| originalString remoteString readSemaphore readString testSemaphore |
	originalString := '"test"'.
	remoteString := nil.
	readSemaphore := Semaphore new.
	testSemaphore := Semaphore new.
	
	self ensureChangesFileOpenedInProcess.

	[	self ensureChangesFileOpenedInProcess.
		
		readSemaphore wait.

		"Write the string, that will be read in other process."
		remoteString := SourceFiles remoteStringForNewString: originalString.

		testSemaphore signal 
		] fork.

	readSemaphore signal.
	testSemaphore wait.

	"Read the string that was written in other process."
	readString := remoteString string.
	
	self assert: readString equals: originalString.
]

{ #category : #testing }
SourceFileArrayTest >> testProtocol [
	"Test that we can access protocol correctly"

	| okCm notOkCm |
	okCm := Point >> #dotProduct:.
	self assert: ((SourceFiles sourcedDataAt: okCm sourcePointer) beginsWith: 'Point methodsFor: ''point functions''').
	self assert: (SourceFiles protocolAt: okCm sourcePointer) equals: 'point functions'.

	notOkCm := Behavior >> #superclass.
	self assert: (SourceFiles protocolAt: notOkCm sourcePointer) equals: 'accessing class hierarchy'
]

{ #category : #testing }
SourceFileArrayTest >> testRemoteStringReadsInGivenSourceFileArray [
	| fs array remoteString readStream |
	fs := FileSystem memory.
	array := SourceFileArray new.
	array changesFileStream: (SourceFile on: 'changes.chunk' potentialLocations: {fs root}) tryOpen.
	array sourcesFileStream: (SourceFile on: 'sources.chunk' potentialLocations: {fs root}) tryOpen.
	remoteString := array remoteStringForNewString: 'test'.
	self assert: remoteString string equals: 'test'.
	self assert: (array fileAt: remoteString sourceFileNumber) equals: array changesFileStream.
	readStream := (fs / 'changes.chunk') readStream.
	readStream position: remoteString position.
	self assert: (ChunkReadStream on: readStream) next equals: 'test'
]

{ #category : #testing }
SourceFileArrayTest >> testRemoteStringWritesInGivenSourceFileArray [
	| fs array |
	fs := FileSystem memory.
	array := SourceFileArray new.
	array
		changesFileStream: (SourceFile on: 'changes.chunk' potentialLocations: {fs root}) tryOpen.
	array
		sourcesFileStream: (SourceFile on: 'sources.chunk' potentialLocations: {fs root}) tryOpen.
	array remoteStringForNewString: 'test'.
	self
		assert: (fs / 'changes.chunk') contents
		equals:
			'
test!'
]

{ #category : #testing }
SourceFileArrayTest >> testSourcePointerFromFileIndexAndPosition [
	"Test valid input ranges"

	| sf |
	sf := SourceFileArray new.
	self should: [ sf sourcePointerFromFileIndex: 0 andPosition: 0 ] raise: Error.
	sf sourcePointerFromFileIndex: 1 andPosition: 0.
	sf sourcePointerFromFileIndex: 2 andPosition: 0.
	self should: [ sf sourcePointerFromFileIndex: 0 andPosition: 3 ] raise: Error.
	self should: [ sf sourcePointerFromFileIndex: 1 andPosition: -1 ] raise: Error.
	sf sourcePointerFromFileIndex: 1 andPosition: 16r1FFFFFF.
	sf sourcePointerFromFileIndex: 1 andPosition: 16r2000000.
	self should: [ sf sourcePointerFromFileIndex: 3 andPosition: 0 ] raise: Error.
	self should: [ sf sourcePointerFromFileIndex: 4 andPosition: 0 ] raise: Error.
	self assert: 16r1000000 equals: (sf sourcePointerFromFileIndex: 1 andPosition: 0).
	self assert: 16r1000013 equals: (sf sourcePointerFromFileIndex: 1 andPosition: 16r13).
	self assert: 16r1FFFFFF equals: (sf sourcePointerFromFileIndex: 1 andPosition: 16rFFFFFF).
	self assert: 16r2000000 equals: (sf sourcePointerFromFileIndex: 2 andPosition: 0).
	self assert: 16r2000013 equals: (sf sourcePointerFromFileIndex: 2 andPosition: 16r13).
	self assert: 16r2FFFFFF equals: (sf sourcePointerFromFileIndex: 2 andPosition: 16rFFFFFF).
	self assert: 16r3000000 equals: (sf sourcePointerFromFileIndex: 1 andPosition: 16r1000000).
	self assert: 16r3000013 equals: (sf sourcePointerFromFileIndex: 1 andPosition: 16r1000013).
	self assert: 16r3FFFFFF equals: (sf sourcePointerFromFileIndex: 1 andPosition: 16r1FFFFFF).
	self assert: 16r4000000 equals: (sf sourcePointerFromFileIndex: 2 andPosition: 16r1000000).
	self assert: 16r4000013 equals: (sf sourcePointerFromFileIndex: 2 andPosition: 16r1000013).
	self assert: 16r4FFFFFF equals: (sf sourcePointerFromFileIndex: 2 andPosition: 16r1FFFFFF)
]

{ #category : #testing }
SourceFileArrayTest >> testSourcesFileAddressRange [
	"Test file position to source pointer address translation for the sources file"

	| sf i p a a2 |
	sf := SourceFileArray new.
	0 to: 16r1FFFFFFF by: 4093 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		i := sf fileIndexFromSourcePointer: a.
		self assert: i equals: 1.
		p := sf filePositionFromSourcePointer: a.
		self assert: p equals: e.
		a2 := sf sourcePointerFromFileIndex: 1 andPosition: p.
		self assert: a2 equals: a ].
	0 to: 16rFFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r1000000 and: 16r1FFFFFF) ].
	16r1000000 to: 16r1FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r3000000 and: 16r3FFFFFF) ].

	16r2000000 to: 16r2FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r5000000 and: 16r5FFFFFF) ].
	16r3000000 to: 16r3FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r7000000 and: 16r7FFFFFF) ].
	16r4000000 to: 16r4FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16r9000000 and: 16r9FFFFFF) ].
	16r5000000 to: 16r5FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16rB000000 and: 16rBFFFFFF) ].
	16r6000000 to: 16r6FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16rD000000 and: 16rDFFFFFF) ].
	16r7000000 to: 16r7FFFFFF by: 811 do: [ :e | 
		a := sf sourcePointerFromFileIndex: 1 andPosition: e.
		self assert: (a between: 16rF000000 and: 16rFFFFFFF) ]
]

{ #category : #testing }
SourceFileArrayTest >> testSourcesFileStream [

	self assert: SourceFiles sourcesFileStream isNotNil
]

{ #category : #testing }
SourceFileArrayTest >> testTimeStamp [
	"Test that we can access timeStamp correctly"

	| okCm |
	okCm := Point>>#dotProduct: .
	self assert: (((SourceFiles sourcedDataAt: okCm sourcePointer)) beginsWith:   
 'Point methodsFor: ''point functions'' stamp: ''''').
	
]

{ #category : #testing }
SourceFileArrayTest >> testWriteSourceWritesInGivenSourceFileArray [
	| fs array |
	fs := FileSystem memory.
	array := SourceFileArray new.
	array
		changesFileStream: (SourceFile on: 'changes.chunk' potentialLocations: {fs root}) tryOpen.
	array
		sourcesFileStream: (SourceFile on: 'sources.chunk' potentialLocations: {fs root}) tryOpen.
	array
		writeSource: 'some source'
		preamble: 'some preamble'
		onSuccess: [ :sourcePointer | "continue with assertions" ]
		onFail: [ "fail the test" self fail ].
	self assert: (fs / 'changes.chunk') contents equals: 'some preamblesome source!'
]

{ #category : #testing }
SourceFileArrayTest >> testWriteSourceWritesInGivenSourceFileArrayWithFlushDefering [
	| fs array |
	fs := FileSystem memory.
	array := SourceFileArray new.
	array
		changesFileStream:
			(SourceFile on: 'changes.chunk' potentialLocations: { fs root })
				tryOpen;
		sourcesFileStream:
			(SourceFile on: 'sources.chunk' potentialLocations: { fs root })
				tryOpen;
		deferFlushDuring: [ 
			array
				writeSource: 'some source'
				preamble: 'some preamble'
				onSuccess: [ :arg1 |  ]
				onFail: [ self fail ] ].
	self
		assert: (fs / 'changes.chunk') contents
		equals: 'some preamblesome source!'
]

{ #category : #testing }
SourceFileArrayTest >> testWriteToBufferedStream [
	| fs array |
	fs := FileSystem memory.
	array := SourceFileArray new.
	
	array changesFileStream: 
		(SourceFileCharacterReadWriteStream on: (SourceFileBufferedReadWriteStream on: (fs binaryWriteStreamOn: (fs / 'changes.chunk') path)) encoding: #utf8).
	
	array sourcesFileStream: 
		(SourceFileCharacterReadWriteStream on: (SourceFileBufferedReadWriteStream on: (fs binaryWriteStreamOn: (fs / 'sources.chunk') path)) encoding: #utf8).

	array
		writeSource: 'some source'
		preamble: 'some preamble'
		onSuccess: [ :sourcePointer | "continue with assertions" ]
		onFail: [ "fail the test" self fail ].
	self assert: (fs / 'changes.chunk') contents equals: 'some preamblesome source!'
]

{ #category : #testing }
SourceFileArrayTest >> testWriteToChangesFileInGivenSourceFileArray [
	| fs array |
	fs := FileSystem memory.
	array := SourceFileArray new.
	array
		changesFileStream: (SourceFile on: 'changes.chunk' potentialLocations: {fs root}) tryOpen.
	array
		sourcesFileStream: (SourceFile on: 'sources.chunk' potentialLocations: {fs root}) tryOpen.
	array
		changesWriteStreamDo: [ :changesFile | 
			changesFile
				cr;
				cr;
				nextChunkPut: 'test';
				cr ].
	self
		assert: (fs / 'changes.chunk') contents
		equals:
			'

test!'
]
