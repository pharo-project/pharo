Class {
	#name : 'ChunkFileFormatParserV200',
	#superclass : 'ChunkFileFormatParserV100',
	#category : 'CodeImport-Utilities',
	#package : 'CodeImport',
	#tag : 'Utilities'
}

{ #category : 'accessing' }
ChunkFileFormatParserV200 class >> writer [
	
	^ CodeExporterV200
]

{ #category : 'parsing' }
ChunkFileFormatParserV200 >> checkCompatibilityWith: versionNumber [

	"There should be at least one component: the major version"
	versionNumber isEmpty ifTrue: [ CodeImportError signal: 'Empty version number' ].

	versionNumber first = self major ifFalse: [
		CodeImportError signal: 'Parser VERSION ' , self versionString , ' cannot read file VERSION ' , ($. join: versionNumber) ]
]

{ #category : 'class factory' }
ChunkFileFormatParserV200 >> doItChunkClass [

	^ DoItChunkV200
]

{ #category : 'parsing' }
ChunkFileFormatParserV200 >> parseAndValidateVersion [
	
	self checkCompatibilityWith: self parseVersion
]

{ #category : 'parsing' }
ChunkFileFormatParserV200 >> parseBehaviorDefinition: aMetadataChunk [
	"The behavior preamble is:
	
	#BehaviorDefinition
	
	Followed by a chunk contaning the Class builder expression"

	| classDefinitionString |
	classDefinitionString := readStream next.
	self addDeclaration: (ClassChunk contents: classDefinitionString)
]

{ #category : 'parsing' }
ChunkFileFormatParserV200 >> parseChunkWithMetadata: nextChunk [

	nextChunk = #PackageDefinition ifTrue: [ ^ self parsePackageDefinition: nextChunk ].
	nextChunk = #BehaviorDefinition ifTrue: [ ^ self parseBehaviorDefinition: nextChunk ].
	^ super parseChunkWithMetadata: nextChunk
]

{ #category : 'parsing' }
ChunkFileFormatParserV200 >> parsePackageDefinition: aMetadataChunk [
	"The package preamble is:
	
	#PackageDefinition
	
	Followed by a chunk contaning the Package name"

	| packageName |
	packageName := readStream next.
	self addDeclaration: (PackageAdditionChunk contents: packageName)
]

{ #category : 'parsing' }
ChunkFileFormatParserV200 >> parseVersion [
	
	| firstChunk prefix versionNumber |
	firstChunk := readStream next.
	
	"The first chunk must be a doit, with a single comment with the form
	
		VERSION:x[.[y[.z]]]
	

	Where x is the major version, y is the minor version and z is the patch version."

	"Check that it's a comment. It starts and ents with a double quote.
	Also, it is prefixed with VERSION: "
	prefix := '"VERSION:'.
	(firstChunk beginsWith: prefix)
		ifFalse: [ CodeImportError signal: 'Incorrect VERSION tag:', firstChunk ].
	(firstChunk last = $") ifFalse: [ CodeImportError signal: 'Incorrect VERSION tag: Comment closer expected at the end of the chunk' ].

	"All components in the version number should be integers separated by dots"
	versionNumber := ((firstChunk allButFirst: prefix size)
		"skip the last double quote of the comment"
		allButLast
		splitOn: $.)
			collect: [ :e | | stream version |
				"If the version is not an integer, ignore and continue to parse an old file"
				stream := e readStream.
				version := (NumberParser on: stream)
					failBlock: [ CodeImportError signal: 'Incorrect VERSION tag:', firstChunk ];
					nextIntegerBase: 10.
				stream atEnd ifFalse: [ CodeImportError signal: 'Incorrect VERSION tag:', firstChunk ].
				version ] as: Array.

	^ versionNumber
]

{ #category : 'versions' }
ChunkFileFormatParserV200 >> version [
	
	^ #( 2 0 0 )
]
