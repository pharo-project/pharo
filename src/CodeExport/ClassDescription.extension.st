Extension { #name : #ClassDescription }

{ #category : #'*CodeExport' }
ClassDescription >> fileOutChangedMessages: aSet on: aFileStream [
	"File a description of the messages of this class that have been
	changed (i.e., are entered into the argument, aSet) onto aFileStream."

	| org |
	(org := self organization) protocolNames do: [ :protocolName |
		(org methodsInProtocolNamed: protocolName)
			select: [ :selector | aSet includes: selector ]
			thenDo: [ :selector | self printMethodChunk: selector on: aFileStream ] ]
]

{ #category : #'*CodeExport' }
ClassDescription >> fileOutLocalMethodsInProtocolNamed: protocolName on: aFileStream [
	"File out methods for this protocol."

	aFileStream cr.

	(self selectorsInProtocolNamed: protocolName)
		do: [ :selector | self printMethodChunk: selector on: aFileStream ]
]

{ #category : #'*CodeExport' }
ClassDescription >> fileOutMethod: selector [
	"Write source code of a single method on a file.  Make up a name for the file."

	| internalStream |
	internalStream := (String new: 1000) writeStream.
	self fileOutMethod: selector on: internalStream.
	CodeExporter writeSourceCodeFrom: internalStream baseName: (self name , '-' , (selector copyReplaceAll: ':' with: '..')) isSt: true
]

{ #category : #'*CodeExport' }
ClassDescription >> fileOutMethod: selector on: aStream [

	selector == #Comment ifTrue: [ ^ self inform: 'Sorry, cannot file out class comment in isolation.' ].
	(self includesSelector: selector) ifFalse: [ ^ self error: 'Selector ' , selector asString , ' not found' ].
	aStream
		header;
		timeStamp.
	self printMethodChunk: selector on: aStream
]

{ #category : #'*CodeExport' }
ClassDescription >> fileOutOn: aFileStream [
	"File a description of the receiver on aFileStream."

	"we for now file out old style class definitions"

	aFileStream nextChunkPut: self oldPharoDefinition.
	self organization putCommentOnFile: aFileStream forClass: self.
	self organization protocolNames do: [ :protocolName | self fileOutLocalMethodsInProtocolNamed: protocolName on: aFileStream ]
]

{ #category : #'*CodeExport' }
ClassDescription >> fileOutOrganizationOn: aFileStream [
	"File a description of the receiver's organization on aFileStream."

	aFileStream
		cr;
		nextPut: $!;
		nextChunkPut: self name , ' reorganize';
		cr;
		nextChunkPut: self organization stringForFileOut;
		cr
]

{ #category : #'*CodeExport' }
ClassDescription >> printMethodChunk: selector on: outStream [
	"Copy the source code for the method associated with selector onto the fileStream."

	| preamble method |
	preamble := self name, ' methodsFor: ', (self organization protocolNameOfElement: selector) asString printString.
	method := self methodDict
		at: selector
		ifAbsent: [
			outStream
				nextPutAll: selector;
				cr.
			outStream
				tab;
				nextPutAll: '** ERROR!  THIS SCRIPT IS MISSING ** ';
				cr;
				cr.
			outStream nextPutAll: '  '.
			^ outStream ].

	"Print the preamble"
	outStream cr; nextChunkPut: ''.
	outStream nextChunkPut: (String streamContents: [ :strm |
		strm nextPutAll: preamble.
		method stamp ifNotEmpty: [ :stamp |
			strm
				nextPutAll: ' stamp: ';
				print: stamp ] ]).
	outStream cr.

	"Print the source code"
	outStream
		nextChunkPut: method sourceCode;
		nextChunkPut: ' '; cr.

	^ outStream
]

{ #category : #'*CodeExport' }
ClassDescription >> printProtocolChunk: protocolName on: aFileStream withStamp: changeStamp priorMethod: priorMethod [
	"Print a method preamble.  This must have a protocol name.
	It may have an author/date stamp, and it may have a prior source link.
	If it has a prior source link, it MUST have a stamp, even if it is empty."

	"The current design is that changeStamps and prior source links are preserved in the changes file.  All fileOuts include changeStamps.  Condensing sources, however, eliminates all stamps (and links, natch)."

	aFileStream
		cr;
		nextPut: $!.
	aFileStream nextChunkPut: (String streamContents: [ :strm |
			 strm
				 nextPutAll: self name;
				 nextPutAll: ' methodsFor: ';
				 print: protocolName asString.
			 (changeStamp isNotNil and: [ changeStamp size > 0 or: [ priorMethod isNotNil ] ]) ifTrue: [
				 strm
					 nextPutAll: ' stamp: ';
					 print: changeStamp ].
			 priorMethod ifNotNil: [
				 strm
					 nextPutAll: ' prior: ';
					 print: priorMethod sourcePointer ] ])
]

{ #category : #'*CodeExport' }
ClassDescription >> selectorsInProtocolNamed: aSymbol [

	^ self organization methodsInProtocolNamed: aSymbol
]
