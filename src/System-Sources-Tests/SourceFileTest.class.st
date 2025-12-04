Class {
	#name : 'SourceFileTest',
	#superclass : 'TestCase',
	#category : 'System-Sources-Tests',
	#package : 'System-Sources-Tests'
}

{ #category : 'tests' }
SourceFileTest >> testCreateFileWithVersion [

	| fs file |
	fs := FileSystem memory.
	file := fs / 'changes.chunk'.
	self deny: file exists.

	file := SourceFile createFileNamed: file version: ChunkFileFormatParserV200.
	file close.
	
	file tryOpen.
	self assert: file nextChunk equals: '"VERSION:2.0.0"'
]

{ #category : 'tests' }
SourceFileTest >> testCreateFileWithVersionDetectsVersion [

	| fs file |
	fs := FileSystem memory.
	file := fs / 'changes.chunk'.
	self deny: file exists.

	(SourceFile createFileNamed: file version: ChunkFileFormatParserV200) close.
	
	file := SourceFile lookupFileNamed: file basename potentialLocations: { file parent }.
	self assert: file formatVersion equals: #(2 0 0)
]

{ #category : 'tests' }
SourceFileTest >> testDetectVersionOfFileVersion1 [

	| fs file |
	fs := FileSystem memory.
	(fs / 'changes.chunk')
		writeStreamDo: [ :s | s nextPutAll: '"VERSION:1.0.0"!' ].
	
	file := SourceFile lookupFileNamed: 'changes.chunk' potentialLocations: { fs root }.
	
	self assert: file formatVersion equals: #(1 0 0)
]

{ #category : 'tests' }
SourceFileTest >> testDetectVersionOfFileVersion2 [

	| fs file |
	fs := FileSystem memory.
	(fs / 'changes.chunk')
		writeStreamDo: [ :s | s nextPutAll: '"VERSION:2.0.0"!' ].
	
	file := SourceFile lookupFileNamed: 'changes.chunk' potentialLocations: { fs root }.
	
	self assert: file formatVersion equals: #(2 0 0)
]

{ #category : 'tests' }
SourceFileTest >> testDetectVersionOfFileWithArbitraryVersion [

	| fs file |
	fs := FileSystem memory.
	(fs / 'changes.chunk')
		writeStreamDo: [ :s | s nextPutAll: '"VERSION:2.3.56"!' ].
	
	file := SourceFile lookupFileNamed: 'changes.chunk' potentialLocations: { fs root }.
	
	self assert: file formatVersion equals: #(2 3 56)
]

{ #category : 'tests' }
SourceFileTest >> testOpenEmptyFileFails [

	| fs file |
	
	"Create the file, it should be empty"
	fs := FileSystem memory.
	(fs / 'changes.chunk') writeStreamDo: [ :s | ].
	self assert: (fs / 'changes.chunk') contents isEmpty.

	file := SourceFile lookupFileNamed: 'changes.chunk' potentialLocations: { fs root }.
	[ file tryOpen. self fail ] on: Error do: [ :err |
		self assert: err messageText equals: 'Cannot open non source file: changes.chunk' ]
]

{ #category : 'tests' }
SourceFileTest >> testOpenNonExistingFileFails [

	| fs file |
	fs := FileSystem memory.
	self deny: (fs / 'changes.chunk') exists.

	file := SourceFile lookupFileNamed: 'changes.chunk' potentialLocations: { fs root }.
	[ file tryOpen. self fail ] on: Error do: [ :err |
		self assert: err messageText equals: 'Cannot open non source file: changes.chunk' ]
]

{ #category : 'tests' }
SourceFileTest >> testWriteClassDefinitionOnFormatV100 [

	| fs file importer declarations |
	fs := FileSystem memory.
	file := fs / 'changes.chunk'.
	self deny: file exists.

	(SourceFile createFileNamed: file version: ChunkFileFormatParserV100) tryOpen newWriter
		successBlock: [ :x | ];
		failBlock: [ self error ];
		writeClassDefinition: 'ASDASD'.

	importer := CodeImporter withParserClass: ChunkFileFormatParserV100.
	file readStreamDo: [ :str |
		importer readStream: str.
		declarations := importer parseChunks
	].
	
	self assert: declarations size equals: 1.
	self assert: declarations first isDoItDeclaration.
	self assert: declarations first contents equals: 'ASDASD'
]

{ #category : 'tests' }
SourceFileTest >> testWriteClassDefinitionOnFormatV200 [

	| fs file importer declarations |
	fs := FileSystem memory.
	file := fs / 'changes.chunk'.
	self deny: file exists.

	(SourceFile createFileNamed: file version: ChunkFileFormatParserV200) tryOpen newWriter
		successBlock: [ :x | ];
		failBlock: [  ];
		writeClassDefinition: 'ASDASD'.
	
	importer := CodeImporter withParserClass: ChunkFileFormatParserV200.
	file readStreamDo: [ :str |
		importer readStream: str.
		declarations := importer parseChunks
	].
	
	self assert: declarations size equals: 1.
	self assert: declarations first isClassDeclaration.
	self assert: declarations first contents equals: 'ASDASD'
]
