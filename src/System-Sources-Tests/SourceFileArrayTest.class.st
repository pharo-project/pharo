"
This test documents the source pointer address conversion methods for ExpandedSourceFileArray.

The available address space for source pointers in a traditional CompiledMethod is 16r1000000 through 16r4FFFFFF. StandardSourceFileArray maps positions in the sources file to address range 16r1000000 through 16r1FFFFFF and 16r3000000 through 16r3FFFFFF, and positions in the changes file to address range 16r2000000 through 16r2FFFFFF and 16r4000000 through 16r4FFFFFF. This permits a maximum file size of 16r2000000 (32MB) for both the sources file and the changes file. 

ExpandedSourceFileArray extends the source pointer address space using bit 25 of the source pointer to identify the external sources and changes files, with the remaining high order bits treated as address extension. This limits the number of external file references to two (the traditional sources and changes files). If additional external file references are needed in the future, some higher order bits in the source pointer address space should be allocated for that purpose.

The use of bit 25 of the source pointer for file references permits backward compatibility with StandardSourceFileArray, with essentially unlimited address space expansion for the sources and changes files.

"
Class {
	#name : 'SourceFileArrayTest',
	#superclass : 'TestCase',
	#category : 'System-Sources-Tests',
	#package : 'System-Sources-Tests'
}

{ #category : 'running' }
SourceFileArrayTest >> ensureChangesFileOpenedInProcess [
	"Ensure the changes file is open"

	| remoteString |
	remoteString := SourceFiles remoteStringAt: thisContext method sourcePointer.

	self deny: remoteString string isNil.
	"self deny: ProcessAndSessionLocalSourcesFileArray value isNil."
]

{ #category : 'tests' }
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

{ #category : 'tests' }
SourceFileArrayTest >> testChangesFileStream [

	self assert: SourceFiles changesFileStream isNotNil
]

{ #category : 'tests' }
SourceFileArrayTest >> testFileIndexFromSourcePointer [

	| sf |
	sf := SourceFileArray new.
	"sources file mapping"
	self assert: 1 equals: (sf fileIndexFromSourcePointer: 16r1000000).
	"changes file mapping"
	self assert: 2 equals: (sf fileIndexFromSourcePointer: 16r2000001)
]

{ #category : 'tests' }
SourceFileArrayTest >> testFilePositionFromSourcePointer [
	"Test derivation of file position for sources or changes file from source pointers"
	
	| sf |
	sf := SourceFileArray new.
	"sources file"
	self assert: 0 equals: (sf filePositionFromSourcePointer: 0).
	self assert: 16r80000 equals: (sf filePositionFromSourcePointer: 16r100000).
	"changes file"
	self assert: 0 equals: (sf filePositionFromSourcePointer: 1).
	self assert: 16r800009 equals: (sf filePositionFromSourcePointer: 16r1000013).
	
]

{ #category : 'tests' }
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

	self assert: readString equals: originalString
]

{ #category : 'tests' }
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

	self assert: readString equals: originalString
]

{ #category : 'tests' }
SourceFileArrayTest >> testParseStampWithConflict [

	| sourceFiles timestamp preamble |
	sourceFiles := SourceFileArray new.

	preamble := 'SourceFileArray methodsFor: ''public - stamp: reading'' stamp: ''1/14/2025 10:16'' prior: 62647896'.
	timestamp := sourceFiles
		             timeStampFromPreamble: preamble
		             for: sourceFiles sourceDataPointers.

	self assert: timestamp equals: '1/14/2025 10:16'
]

{ #category : 'tests' }
SourceFileArrayTest >> testParseStampWithNoStamp [

	| sourceFiles timestamp preamble |
	sourceFiles := SourceFileArray new.

	preamble := 'AeBenchBlurConvexPolygonRunner methodsFor: ''running'''.
	timestamp := sourceFiles
		             timeStampFromPreamble: preamble
		             for: sourceFiles sourceDataPointers.

	self assert: timestamp equals: ''
]

{ #category : 'tests' }
SourceFileArrayTest >> testParseStampWithoutConflict [

	| sourceFiles timestamp preamble |
	sourceFiles := SourceFileArray new.

	preamble := 'SourceFileArray methodsFor: ''public - string reading'' stamp: ''1/14/2025 10:16'' prior: 62647896'.
	timestamp := sourceFiles
		             timeStampFromPreamble: preamble
		             for: sourceFiles sourceDataPointers.

	self assert: timestamp equals: '1/14/2025 10:16'
]

{ #category : 'tests' }
SourceFileArrayTest >> testProtocol [
	"Test that we can access protocol correctly"

	| okCm notOkCm |
	okCm := Point >> #dotProduct:.
	self assert: ((SourceFiles sourcedDataAt: okCm sourcePointer) beginsWith: 'Point methodsFor: ''point functions''').
	self assert: (SourceFiles protocolAt: okCm sourcePointer) equals: 'point functions'.

	notOkCm := Behavior >> #superclass.
	self assert: (SourceFiles protocolAt: notOkCm sourcePointer) equals: 'accessing - class hierarchy'
]

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
SourceFileArrayTest >> testSourcePointerFromFileIndexAndPosition [

	| sf |
	sf := SourceFileArray new.
	self should: [ sf sourcePointerFromFileIndex: 0 andPosition: 0 ] raise: Error.
	self assert: (sf sourcePointerFromFileIndex: 1 andPosition: 0) equals: 0.
	self assert: (sf sourcePointerFromFileIndex: 2 andPosition: 0) equals: 1.
	self should: [ sf sourcePointerFromFileIndex: 0 andPosition: 3 ] raise: Error.
	self should: [ sf sourcePointerFromFileIndex: 1 andPosition: -1 ] raise: Error.
	self assert: (sf sourcePointerFromFileIndex: 1 andPosition: 16r1FFFFFF) equals: 67108862.
	self assert: (sf sourcePointerFromFileIndex: 1 andPosition: 16r2000000) equals:  67108864.
	self should: [ sf sourcePointerFromFileIndex: 3 andPosition: 0 ] raise: Error.
	self should: [ sf sourcePointerFromFileIndex: 4 andPosition: 0 ] raise: Error.

]

{ #category : 'tests' }
SourceFileArrayTest >> testSourcesFileStream [

	self assert: SourceFiles sourcesFileStream isNotNil
]

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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
