"
A ChunkFileFormatParserV200Test is a test class for testing the behavior of ChunkFileFormatParserV200
"
Class {
	#name : 'ChunkFileFormatParserV100Test',
	#superclass : 'TestCase',
	#category : 'CodeImport-Tests-Utilities',
	#package : 'CodeImport-Tests',
	#tag : 'Utilities'
}

{ #category : 'Protocol (tests) - 2 selector(s)' }
ChunkFileFormatParserV100Test >> testParseClassDefinition [

	| parser declarations oldDefinition input |
	"Make sure we input the old definition.
	We add a new line and a bang to make it a chunk"
	oldDefinition := ClassDefinitionPrinter oldPharo
		for: self class;
		definitionString.
	input := '
', oldDefinition, '!'.
	
	parser := ChunkFileFormatParserV100 for: input readStream.
	
	declarations := parser parseChunks.
	self assert: declarations size equals: 1.
	self assert: declarations first isDoItDeclaration.
	self assert: declarations first contents equals: oldDefinition.
	self assert: declarations first isClassAdditionChunk
]

{ #category : 'Protocol (tests) - 2 selector(s)' }
ChunkFileFormatParserV100Test >> testParseDoItDefinition [

	| parser declarations input |
	input := '
arbitraty text is just a do it....!'.
	
	parser := ChunkFileFormatParserV100 for: input readStream.
	
	declarations := parser parseChunks.
	self assert: declarations size equals: 1.
	self assert: declarations first isDoItDeclaration.
	self assert: declarations first contents equals: 'arbitraty text is just a do it....'.
	
	self deny: declarations first isClassAdditionChunk.
	self deny: declarations first isPackageAdditionChunk
]

{ #category : 'Protocol (tests) - 2 selector(s)' }
ChunkFileFormatParserV100Test >> testParseMethodDefinitionWithMetadata [

	| parser declarations input |
	input := '
!MyClass methodsFor: ''toto'' stamp: ''wat''!
foo!metadata!'.
	
	parser := ChunkFileFormatParserV100 for: input readStream.
	
	declarations := parser parseChunks.
	self assert: declarations size equals: 1.
	self assert: declarations first isMethodDeclaration.

	self assert: declarations first protocol equals: 'toto'.
	self assert: declarations first stamp equals: 'wat'.
	self assert: declarations first contents equals: 'foo'.
	self assert: declarations first metadata equals: 'metadata'
]

{ #category : 'Protocol (tests) - 2 selector(s)' }
ChunkFileFormatParserV100Test >> testParseMethodDefinitionWithTimestamp [

	| parser declarations input |
	input := '
!MyClass methodsFor: ''toto'' stamp: ''wat''!
foo'.
	
	parser := ChunkFileFormatParserV100 for: input readStream.
	
	declarations := parser parseChunks.
	self assert: declarations size equals: 1.
	self assert: declarations first isMethodDeclaration.

	self assert: declarations first protocol equals: 'toto'.
	self assert: declarations first stamp equals: 'wat'.
	self assert: declarations first contents equals: 'foo'
]

{ #category : 'Protocol (tests) - 2 selector(s)' }
ChunkFileFormatParserV100Test >> testParseMethodDefinitionWithoutTimestamp [

	| parser declarations input |
	input := '
!MyClass methodsFor: ''toto''!
foo'.
	
	parser := ChunkFileFormatParserV100 for: input readStream.
	
	declarations := parser parseChunks.
	self assert: declarations size equals: 1.
	self assert: declarations first isMethodDeclaration.

	self assert: declarations first protocol equals: 'toto'.
	self assert: declarations first stamp equals: ''.
	self assert: declarations first contents equals: 'foo'
]

{ #category : 'tests' }
ChunkFileFormatParserV100Test >> testParsePackageDefinition [

	| parser declarations |
	parser := ChunkFileFormatParserV100 for: '
SystemOrganization addCategory: #''CodeExport-Tests''!' readStream.
	
	declarations := parser parseChunks.
	self assert: declarations size equals: 1.
	self assert: declarations first isDoItDeclaration.
	self assert: declarations first contents equals: 'SystemOrganization addCategory: #''CodeExport-Tests'''.
	self assert: declarations first isPackageAdditionChunk
]
