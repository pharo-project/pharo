"
This test documents the source pointer address conversion methods for ExpandedSourceFileArray.

The available address space for source pointers in a traditional CompiledMethod is 16r1000000 through 16r4FFFFFF. StandardSourceFileArray maps positions in the sources file to address range 16r1000000 through 16r1FFFFFF and 16r3000000 through 16r3FFFFFF, and positions in the changes file to address range 16r2000000 through 16r2FFFFFF and 16r4000000 through 16r4FFFFFF. This permits a maximum file size of 16r2000000 (32MB) for both the sources file and the changes file. 

ExpandedSourceFileArray extends the source pointer address space using bit 25 of the source pointer to identify the external sources and changes files, with the remaining high order bits treated as address extension. This limits the number of external file references to two (the traditional sources and changes files). If additional external file references are needed in the future, some higher order bits in the source pointer address space should be allocated for that purpose.

The use of bit 25 of the source pointer for file references permits backward compatibility with StandardSourceFileArray, with essentially unlimited address space expansion for the sources and changes files.

"
Class {
	#name : 'FilesSourcesManagerTest',
	#superclass : 'TestCase',
	#category : 'System-Sources-Tests',
	#package : 'System-Sources-Tests'
}

{ #category : 'tests' }
FilesSourcesManagerTest >> ensureChangesFileOpenedInProcess [
	"Ensure the changes file is open"

	| sourcePointer fileIndex filePosition string |
	sourcePointer := thisContext method sourcePointer.
	fileIndex := self globalFilesSourcesManager fileIndexFromSourcePointer: sourcePointer.
	filePosition := self globalFilesSourcesManager filePositionFromSourcePointer: sourcePointer.

	string := self globalFilesSourcesManager
		          readStreamAtFileIndex: fileIndex
		          atPosition: filePosition
		          ifPresent: [ :readStream | (ChunkReadStream on: readStream) next ]
		          ifAbsent: [ nil ].

	self deny: string isNil
]

{ #category : 'helpers' }
FilesSourcesManagerTest >> globalFilesSourcesManager [
	"This is bad. Tests should not use a global instance"

	SourcesManager current class = FilesSourcesManager ifFalse: [ self error: 'This test can only pass if the sources manager is the FilesSourcesManager.' ].
	^ SourcesManager current
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testAddressRange [
	"Test source pointer to file position address translation across a wide address range"

	| sf i p a |
	sf := FilesSourcesManager new.
	16r1000000 to: 16r10000000 by: 4093 do: [ :e |
		i := sf fileIndexFromSourcePointer: e.
		p := sf filePositionFromSourcePointer: e.
		a := sf sourcePointerFromFileIndex: i andPosition: p.
		self assert: a equals: e ]
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testChangesFileStream [

	self assert: self globalFilesSourcesManager changesFileStream isNotNil
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testFileIndexFromSourcePointer [

	| sf |
	sf := FilesSourcesManager new.
	"sources file mapping"
	self assert: 1 equals: (sf fileIndexFromSourcePointer: 16r1000000).
	"changes file mapping"
	self assert: 2 equals: (sf fileIndexFromSourcePointer: 16r2000001)
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testFilePositionFromSourcePointer [
	"Test derivation of file position for sources or changes file from source pointers"
	
	| sf |
	sf := FilesSourcesManager new.
	"sources file"
	self assert: 0 equals: (sf filePositionFromSourcePointer: 0).
	self assert: 16r80000 equals: (sf filePositionFromSourcePointer: 16r100000).
	"changes file"
	self assert: 0 equals: (sf filePositionFromSourcePointer: 1).
	self assert: 16r800009 equals: (sf filePositionFromSourcePointer: 16r1000013).
	
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testForkedRead [

	| originalString remoteString readSemaphore readString testSemaphore position |
	originalString := '"test"'.
	remoteString := nil.
	readSemaphore := Semaphore new.
	testSemaphore := Semaphore new.

	self ensureChangesFileOpenedInProcess.

	[	self ensureChangesFileOpenedInProcess.

		readSemaphore wait.

		"Read the string that was written in other process."
		readString := self globalFilesSourcesManager
			readStreamAtFileIndex: 2 "changes"
			atPosition: position
			ifPresent: [ :readStream | (ChunkReadStream on: readStream) next ]
			ifAbsent: [ nil ].
		testSemaphore signal
		] fork.

	"Write the string, that will be read in other process."
	self globalFilesSourcesManager writeChangesDuring: [ :theFile |
		theFile cr.
		position := theFile position.
		(ChunkWriteStream on: theFile) nextPut: originalString.
		theFile position
	] onSuccess: [ :sourcePointer | "Ignore" ] onFail: [ "Ignore" ].

	readSemaphore signal.
	testSemaphore wait.

	self assert: readString equals: originalString
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testForkedWrite [

	| originalString remoteString readSemaphore readString testSemaphore position |
	originalString := '"test"'.
	remoteString := nil.
	readSemaphore := Semaphore new.
	testSemaphore := Semaphore new.

	self ensureChangesFileOpenedInProcess.

	[	self ensureChangesFileOpenedInProcess.

		readSemaphore wait.

		"Write the string, that will be read in other process."
		self globalFilesSourcesManager writeChangesDuring: [ :theFile |
			theFile cr.
			position := theFile position.
			(ChunkWriteStream on: theFile) nextPut: originalString.
			position
		] onSuccess: [ :sourcePointer | "Ignore" ] onFail: [ "Ignore" ].

		testSemaphore signal
		] fork.

	readSemaphore signal.
	testSemaphore wait.

	"Read the string that was written in other process."
	readString := self globalFilesSourcesManager
		readStreamAtFileIndex: 2 "changes"
		atPosition: position
		ifPresent: [ :readStream | (ChunkReadStream on: readStream) next ]
		ifAbsent: [ nil ].

	self assert: readString equals: originalString
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testParseStampWithConflict [

	| sourceFiles timestamp preamble |
	sourceFiles := FilesSourcesManager new.

	preamble := 'FilesSourcesManager methodsFor: ''public - stamp: reading'' stamp: ''1/14/2025 10:16'' prior: 62647896'.
	timestamp := sourceFiles
		             timeStampFromPreamble: preamble
		             for: sourceFiles sourceDataPointers.

	self assert: timestamp equals: '1/14/2025 10:16'
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testParseStampWithNoStamp [

	| sourceFiles timestamp preamble |
	sourceFiles := FilesSourcesManager new.

	preamble := 'AeBenchBlurConvexPolygonRunner methodsFor: ''running'''.
	timestamp := sourceFiles
		             timeStampFromPreamble: preamble
		             for: sourceFiles sourceDataPointers.

	self assert: timestamp equals: ''
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testParseStampWithoutConflict [

	| sourceFiles timestamp preamble |
	sourceFiles := FilesSourcesManager new.

	preamble := 'FilesSourcesManager methodsFor: ''public - string reading'' stamp: ''1/14/2025 10:16'' prior: 62647896'.
	timestamp := sourceFiles
		             timeStampFromPreamble: preamble
		             for: sourceFiles sourceDataPointers.

	self assert: timestamp equals: '1/14/2025 10:16'
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testProtocol [
	"Test that we can access protocol correctly"

	| okCm notOkCm |
	okCm := Point >> #dotProduct:.
	self assert: ((self globalFilesSourcesManager sourcedDataAt: okCm sourcePointer) beginsWith: 'Point methodsFor: ''point functions''').
	self assert: (self globalFilesSourcesManager protocolAt: okCm sourcePointer) equals: 'point functions'.

	notOkCm := Behavior >> #superclass.
	self assert: (self globalFilesSourcesManager protocolAt: notOkCm sourcePointer) equals: 'accessing - class hierarchy'
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testSourcePointerFromFileIndexAndPosition [

	| sf |
	sf := FilesSourcesManager new.
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
FilesSourcesManagerTest >> testSourcesFileStream [

	self assert: self globalFilesSourcesManager sourcesFileStream isNotNil
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testWriteSourceWritesInGivenSourceFileArray [

	| fs array |
	fs := FileSystem memory.
	array := FilesSourcesManager new.
	array changesFileStream: (SourceFile createFileNamed: fs root / 'changes.chunk' version: ChunkFileFormatParserV200) tryOpen.
	array sourcesFileStream: (SourceFile createFileNamed: fs root / 'sources.chunk' version: ChunkFileFormatParserV200) tryOpen.

	array
		writeChangesDuring: [ :file |
				file nextPutAll: '
!some preamble!
some source!
 !'.
				file position ]
		onSuccess: [ :sourcePointer | "continue with assertions" ]
		onFail: [ "fail the test" self fail ].

	self assert: (fs / 'changes.chunk') contents equals: '"VERSION:2.0.0"!
!some preamble!
some source!
 !'
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testWriteSourceWritesInGivenSourceFileArrayWithFlushDefering [

	| fs array |
	fs := FileSystem memory.
	array := FilesSourcesManager new.
	array
		changesFileStream: (SourceFile createFileNamed: fs root / 'changes.chunk' version: ChunkFileFormatParserV200) tryOpen;
		sourcesFileStream: (SourceFile createFileNamed: fs root / 'sources.chunk' version: ChunkFileFormatParserV200) tryOpen;
		deferFlushDuring: [
				array
					writeChangesDuring: [ :file |
							file nextPutAll: '
!some preamble!
some source!
 !'.
							file position ]
					onSuccess: [ :sourcePointer | "continue with assertions" ]
					onFail: [ "fail the test" self fail ] ].
	self assert: (fs / 'changes.chunk') contents equals: '"VERSION:2.0.0"!
!some preamble!
some source!
 !'
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testWriteToBufferedStream [

	| fs array |
	fs := FileSystem memory.
	array := FilesSourcesManager new.
	
	array changesFileStream: (SourceFile createFileNamed: fs root / 'changes.chunk' version: ChunkFileFormatParserV200) tryOpen.
	array sourcesFileStream: (SourceFile createFileNamed: fs root / 'sources.chunk' version: ChunkFileFormatParserV200) tryOpen.

	array
		writeChangesDuring: [ :file |
				file nextPutAll: '
!some preamble!
some source!
 !'.
				file position ]
		onSuccess: [ :sourcePointer | "continue with assertions" ]
		onFail: [ "fail the test" self fail ].

	self assert: (fs / 'changes.chunk') contents equals: '"VERSION:2.0.0"!
!some preamble!
some source!
 !'
]

{ #category : 'tests' }
FilesSourcesManagerTest >> testWriteToChangesFileInGivenSourceFileArray [

	| fs array |
	fs := FileSystem memory.
	array := FilesSourcesManager new.
	array changesFileStream: (SourceFile createFileNamed: fs root / 'changes.chunk' version: ChunkFileFormatParserV200) tryOpen.
	array sourcesFileStream: (SourceFile createFileNamed: fs root / 'sources.chunk' version: ChunkFileFormatParserV200) tryOpen.
	array writeChangesDuring: [ :changesFile | | position |
			position := changesFile position.
			changesFile
				cr;
				cr.
			(ChunkWriteStream on: changesFile) nextPut: 'test'.
			changesFile cr.
			position. ] onSuccess: [ :sourcePointer | "Ignore" ] onFail: [ "Ignore" ].
	self assert: (fs / 'changes.chunk') contents equals: '"VERSION:2.0.0"!

test!
'
]
