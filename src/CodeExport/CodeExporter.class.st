"
An utility class for exporting code to changesets (*.cs) or Smalltalk source (*.st) files
"
Class {
	#name : 'CodeExporter',
	#superclass : 'Object',
	#instVars : [
		'writeStream'
	],
	#category : 'CodeExport-Base',
	#package : 'CodeExport',
	#tag : 'Base'
}

{ #category : 'instance creation' }
CodeExporter class >> onMemory [

	^ self onStream: (String new: 100) writeStream
]

{ #category : 'instance creation' }
CodeExporter class >> onStream: aStream [

	^ self new
		  writeStream: aStream;
		  yourself
]

{ #category : 'writing' }
CodeExporter >> writeClassCommentOf: aClass [

	"Print header""Print comment. If empty, we need to put an empty line anyways to avoid breaking the parser""Print metadata"
	aClass comment ifNil: [ ^ self ].
	self
		writeComment: aClass comment string
		ofClass: aClass
		stamp: aClass commentStamp
		withMetadata: ' '
		priorSourcePointer: '0'
]

{ #category : 'writing' }
CodeExporter >> writeClassDefinition: aClass [ 

	writeStream cr.
	writeStream nextPut: aClass oldPharoDefinition
]

{ #category : 'writing' }
CodeExporter >> writeClassInitializer: aClass [ 

	writeStream cr.
	writeStream nextPut: aClass name, ' initialize'
]

{ #category : 'writing' }
CodeExporter >> writeComment: commentString ofClass: aClass stamp: commentStamp withMetadata: commentMetadata priorSourcePointer: priorSourcePointer [

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
CodeExporter >> writeMethod: aMethod [

	^ self writeMethod: aMethod metadata: '' prior: 0
]

{ #category : 'writing' }
CodeExporter >> writeMethod: aMethod metadata: metadata prior: priorSourcePointer [

	^ self
		  writeMethodWithClass: aMethod methodClass
		  selector: aMethod selector
		  source: aMethod sourceCode
		  protocol: (aMethod methodClass protocolOfSelector: aMethod selector) name asString 
		  stamp: DateAndTime now asString
		  withMetadata: metadata
		  priorSourcePointer: priorSourcePointer
]

{ #category : 'writing' }
CodeExporter >> writeMethodWithClass: methodClass selector: methodSelector source: methodSourceCode protocol: protocol stamp: methodStamp withMetadata: metadata priorSourcePointer: priorSourcePointer [

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

{ #category : 'accessing' }
CodeExporter >> writeStream [
	^ writeStream
]

{ #category : 'writing' }
CodeExporter >> writeStream: aWriteStream [ 
	
	writeStream := ChunkWriteStream on: aWriteStream
]
