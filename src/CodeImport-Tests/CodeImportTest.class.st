Class {
	#name : 'CodeImportTest',
	#superclass : 'TestCase',
	#category : 'CodeImport-Tests',
	#package : 'CodeImport-Tests'
}

{ #category : 'tests' }
CodeImportTest >> testVersion1FailsWhenLoadingFilesWithVersion2 [

	| importer |
	importer := CodeImporter withParserClass: ChunkFileFormatParserV100.
	importer readStream: '"VERSION:2.0"!1+1!' readStream.
	
	[ importer parseChunks. self fail ]
		on: CodeImportError do: [ :err | self assert: err messageText equals: 'Parser VERSION 1.0.0 cannot read file VERSION 2.0' ]
]

{ #category : 'tests' }
CodeImportTest >> testVersion1LoadsFilesWithIncorrectVersionTag [

	| importer firstChunk chunks secondChunk |
	importer := CodeImporter withParserClass: ChunkFileFormatParserV100.
	importer readStream: '"VERSION:masdjhagd"!1+1!' readStream.
	
	chunks := importer parseChunks.
	firstChunk := chunks first.
	self assert: firstChunk isDoItDeclaration.
	self assert: firstChunk contents equals: '"VERSION:masdjhagd"'.
	
	secondChunk := chunks second.
	self assert: secondChunk isDoItDeclaration.
	self assert: secondChunk contents equals: '1+1'
]

{ #category : 'tests' }
CodeImportTest >> testVersion1LoadsFilesWithVersion1 [

	| importer doitChunk |
	importer := CodeImporter withParserClass: ChunkFileFormatParserV100.
	importer readStream: '"VERSION:1"!1+1!' readStream.
	
	doitChunk := importer parseChunks first.
	self assert: doitChunk isDoItDeclaration.
	self assert: doitChunk contents equals: '1+1'
]

{ #category : 'tests' }
CodeImportTest >> testVersion1LoadsFilesWithVersion10 [

	| importer doitChunk |
	importer := CodeImporter withParserClass: ChunkFileFormatParserV100.
	importer readStream: '"VERSION:1.0"!1+1!' readStream.
	
	doitChunk := importer parseChunks first.
	self assert: doitChunk isDoItDeclaration.
	self assert: doitChunk contents equals: '1+1'
]

{ #category : 'tests' }
CodeImportTest >> testVersion1LoadsFilesWithVersion100 [

	| importer doitChunk |
	importer := CodeImporter withParserClass: ChunkFileFormatParserV100.
	importer readStream: '"VERSION:1.0.0"!1+1!' readStream.
	
	doitChunk := importer parseChunks first.
	self assert: doitChunk isDoItDeclaration.
	self assert: doitChunk contents equals: '1+1'
]

{ #category : 'tests' }
CodeImportTest >> testVersion1LoadsFilesWithoutVersion [

	| importer firstChunk |
	importer := CodeImporter withParserClass: ChunkFileFormatParserV100.
	importer readStream: '1+1!' readStream.
	
	firstChunk := importer parseChunks first.
	self assert: firstChunk isDoItDeclaration.
	self assert: firstChunk contents equals: '1+1'
]

{ #category : 'tests' }
CodeImportTest >> testVersion2FailsFilesWithIncorrectVersionTag [

	| importer |
	importer := CodeImporter withParserClass: ChunkFileFormatParserV200.
	importer readStream: '"VERSION:masdjhagd"!1+1!' readStream.
	
	[ importer parseChunks. self fail ]
		on: CodeImportError do: [ :err | self assert: err messageText equals: 'Incorrect VERSION tag:"VERSION:masdjhagd"' ]
]

{ #category : 'tests' }
CodeImportTest >> testVersion2FailsFilesWithUnclosedVersionTag [

	| importer |
	importer := CodeImporter withParserClass: ChunkFileFormatParserV200.
	
	"Version 2.0.0 is accepted, but it's missing the closing comment quote"
	importer readStream: '"VERSION:2.0.0!1+1!' readStream.
	
	[ importer parseChunks. self fail ]
		on: CodeImportError do: [ :err | self assert: err messageText equals: 'Incorrect VERSION tag: Comment closer expected at the end of the chunk' ]
]

{ #category : 'tests' }
CodeImportTest >> testVersion2FailsWhenLoadingFilesWithVersion1 [

	| importer |
	importer := CodeImporter withParserClass: ChunkFileFormatParserV200.
	importer readStream: '"VERSION:1.0"!1+1!' readStream.

	[ importer parseChunks. self fail ]
		on: CodeImportError do: [ :err | self assert: err messageText equals: 'Parser VERSION 2.0.0 cannot read file VERSION 1.0' ]
]

{ #category : 'tests' }
CodeImportTest >> testVersion2LoadsFilesWithVersion2 [

	| importer doitChunk |
	importer := CodeImporter withParserClass: ChunkFileFormatParserV200.
	importer readStream: '"VERSION:2"!1+1!' readStream.
	
	doitChunk := importer parseChunks first.
	self assert: doitChunk isDoItDeclaration.
	self assert: doitChunk contents equals: '1+1'
]

{ #category : 'tests' }
CodeImportTest >> testVersion2LoadsFilesWithVersion20 [

	| importer doitChunk |
	importer := CodeImporter withParserClass: ChunkFileFormatParserV200.
	importer readStream: '"VERSION:2.0"!1+1!' readStream.
	
	doitChunk := importer parseChunks first.
	self assert: doitChunk isDoItDeclaration.
	self assert: doitChunk contents equals: '1+1'
]

{ #category : 'tests' }
CodeImportTest >> testVersion2LoadsFilesWithVersion200 [

	| importer doitChunk |
	importer := CodeImporter withParserClass: ChunkFileFormatParserV200.
	importer readStream: '"VERSION:2.0.0"!1+1!' readStream.
	
	doitChunk := importer parseChunks first.
	self assert: doitChunk isDoItDeclaration.
	self assert: doitChunk contents equals: '1+1'
]
