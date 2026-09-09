"
An utility class for exporting code to changesets (*.cs) or Smalltalk source (*.st) files
"
Class {
	#name : 'CodeExporterV100',
	#superclass : 'Object',
	#instVars : [
		'writeStream'
	],
	#category : 'CodeExport-Base',
	#package : 'CodeExport',
	#tag : 'Base'
}

{ #category : 'instance creation' }
CodeExporterV100 class >> onMemory [

	^ self onStream: (String new: 100) writeStream
]

{ #category : 'instance creation' }
CodeExporterV100 class >> onStream: aStream [

	^ self new
		  writeStream: aStream;
		  yourself
]

{ #category : 'writing' }
CodeExporterV100 >> writeClassCommentOf: aClass [

	"Print header""Print comment. If empty, we need to put an empty line anyways to avoid breaking the parser""Print metadata"
	aClass comment ifNil: [ ^ self ].
	self
		writeComment: aClass comment string
		ofClass: aClass
		stamp: aClass commentStamp
		withMetadata: ' '
		priorSourcePointer: '0'
]

{ #category : 'writing - low level' }
CodeExporterV100 >> writeClassDefinition: aClassDefinition [

	writeStream cr.
	writeStream nextPut: aClassDefinition
]

{ #category : 'writing' }
CodeExporterV100 >> writeClassDefinitionOf: aClass [ 

	self writeClassDefinition: aClass oldPharoDefinition
]

{ #category : 'writing' }
CodeExporterV100 >> writeClassInitializer: aClass [ 

	writeStream cr.
	writeStream nextPut: aClass name, ' initialize'
]

{ #category : 'writing - low level' }
CodeExporterV100 >> writeComment: commentString ofClass: aClass stamp: commentStamp withMetadata: commentMetadata priorSourcePointer: priorSourcePointer [

	| header sourceCodePosition |
	header := String streamContents: [ :strm |
			          strm
				          nextPutAll: aClass name;
				          nextPutAll: ' commentStamp: '.
			          commentStamp storeOn: strm.
			          strm
				          nextPutAll: ' prior: ';
				          nextPutAll: priorSourcePointer printString ]. "Print header"

	writeStream
		cr;
		bang;
		nextPut: header;
		cr. "Print comment. If empty, we need to put an empty line anyways to avoid breaking the parser"
	
	sourceCodePosition := writeStream position.
		
	"Print the source code"
	writeStream
		nextPut: (commentString ifEmpty: [ String cr ]);
		cr.

	^ sourceCodePosition
]

{ #category : 'writing' }
CodeExporterV100 >> writeFileoutVersion [

	"Write the current version as a doit chunk with a comment.
	This ensures backwards compatibility, as any version should be able to read do-its with comments."
	writeStream nextPut: '"VERSION:1.0"'
]

{ #category : 'writing' }
CodeExporterV100 >> writeMethod: aMethod [

	^ self writeMethod: aMethod metadata: '' prior: 0
]

{ #category : 'writing' }
CodeExporterV100 >> writeMethod: aMethod metadata: metadata prior: priorSourcePointer [

	^ self
		  writeMethodWithClass: aMethod methodClass
		  selector: aMethod selector
		  source: aMethod sourceCode
		  protocol: (aMethod methodClass protocolOfSelector: aMethod selector) name asString 
		  stamp: DateAndTime now asString
		  withMetadata: metadata
		  priorSourcePointer: priorSourcePointer
]

{ #category : 'writing - low level' }
CodeExporterV100 >> writeMethodWithClass: methodClass selector: methodSelector source: methodSourceCode protocol: protocol stamp: methodStamp withMetadata: metadata priorSourcePointer: priorSourcePointer [

	| preamble sourceCodePosition |
	preamble := methodClass name , ' methodsFor: ' , protocol printString.
	methodStamp ifNotEmpty: [
		preamble := preamble, ' stamp: ', methodStamp printString ].
	preamble := preamble, ' prior: ', priorSourcePointer printString.
	writeStream
		cr;
		cr;
		bang;
		nextPut: preamble;
		cr.	

	sourceCodePosition := writeStream position.
	
	"Print the source code"
	writeStream		
		nextPut: methodSourceCode;
		"Print the metadata"
		nextPut: (metadata ifEmpty: [ ' ' ]).
		
	^ sourceCodePosition
]

{ #category : 'writing' }
CodeExporterV100 >> writePackageDefinitionOf: aPackage [

	writeStream cr.
	writeStream nextPut: 'SystemOrganization addCategory: ', aPackage name printString
]

{ #category : 'accessing' }
CodeExporterV100 >> writeStream [
	^ writeStream
]

{ #category : 'private' }
CodeExporterV100 >> writeStream: aWriteStream [ 
	
	writeStream := ChunkWriteStream on: aWriteStream
]
