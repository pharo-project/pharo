"
A ChunkFileFormatParserV200Test is a test class for testing the behavior of ChunkFileFormatParserV200
"
Class {
	#name : 'ChunkFileFormatParserV200Test',
	#superclass : 'TestCase',
	#category : 'CodeImport-Tests-Utilities',
	#package : 'CodeImport-Tests',
	#tag : 'Utilities'
}

{ #category : 'tests' }
ChunkFileFormatParserV200Test >> testParseClassDeclarationWithoutHeaderIsADoIt [

	| parser declarations input fluidDefinition |
	fluidDefinition := ClassDefinitionPrinter fluid
		for: self class;
		definitionString.

	input := '"VERSION:2.0.0"!
', fluidDefinition ,'!'.
	
	parser := ChunkFileFormatParserV200 for: input readStream.
	
	declarations := parser parseChunks.
	self assert: declarations size equals: 1.
	self assert: declarations first isDoItDeclaration.
	self assert: declarations first contents equals: fluidDefinition
]

{ #category : 'tests' }
ChunkFileFormatParserV200Test >> testParseClassDefinition [

	| parser declarations fluidDefinition input |
	"Make sure we input the fluid definition.
	We add a version chunk and a header chunk to make it a class declaration"
	fluidDefinition := ClassDefinitionPrinter fluid
		for: self class;
		definitionString.
	input := '"VERSION:2.0.0"!
!BehaviorDefinition!
', fluidDefinition, '!'.
	
	parser := ChunkFileFormatParserV200 for: input readStream.
	
	declarations := parser parseChunks.
	self assert: declarations size equals: 1.
	self assert: declarations first isClassDeclaration.
	self assert: declarations first contents equals: fluidDefinition
]

{ #category : 'tests' }
ChunkFileFormatParserV200Test >> testParseDoItDefinition [

	| parser declarations input |
	input := '"VERSION:2.0.0"!
arbitraty text is just a do it....!'.
	
	parser := ChunkFileFormatParserV200 for: input readStream.
	
	declarations := parser parseChunks.
	self assert: declarations size equals: 1.
	self assert: declarations first isDoItDeclaration.
	self assert: declarations first contents equals: 'arbitraty text is just a do it....'
]

{ #category : 'tests' }
ChunkFileFormatParserV200Test >> testParseMethodDefinitionWithMetadata [

	| parser declarations input |
	input := '"VERSION:2.0.0"!
!MyClass methodsFor: ''toto'' stamp: ''wat''!
foo!metadata!'.
	
	parser := ChunkFileFormatParserV200 for: input readStream.
	
	declarations := parser parseChunks.
	self assert: declarations size equals: 1.
	self assert: declarations first isMethodDeclaration.

	self assert: declarations first protocol equals: 'toto'.
	self assert: declarations first stamp equals: 'wat'.
	self assert: declarations first contents equals: 'foo'.
	self assert: declarations first metadata equals: 'metadata'
]

{ #category : 'tests' }
ChunkFileFormatParserV200Test >> testParsePackageDefinition [

	| parser declarations input |
	input := '"VERSION:2.0.0"!
!PackageDefinition!
Toto-This-Lala!'.
	
	parser := ChunkFileFormatParserV200 for: input readStream.
	
	declarations := parser parseChunks.
	self assert: declarations size equals: 1.
	self assert: declarations first isPackageDeclaration.
	self assert: declarations first contents equals: 'Toto-This-Lala'
]
