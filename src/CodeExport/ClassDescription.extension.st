Extension { #name : 'ClassDescription' }

{ #category : '*CodeExport' }
ClassDescription >> fileOutChangedMessages: aSet on: aFileStream [
	"File a description of the messages of this class that have been
	changed (i.e., are entered into the argument, aSet) onto aFileStream."

	self protocols do: [ :protocol |
		protocol methodSelectors
			select: [ :selector | aSet includes: selector ]
			thenDo: [ :selector | self printMethodChunk: selector on: aFileStream ] ]
]

{ #category : '*CodeExport' }
ClassDescription >> fileOutLocalMethodsInProtocol: protocol on: aFileStream [
	"File out methods for this protocol."

	aFileStream cr.

	protocol methodSelectors do: [ :selector | self printMethodChunk: selector on: aFileStream ]
]

{ #category : '*CodeExport' }
ClassDescription >> fileOutMethod: selector [
	"Write source code of a single method on a file.  Make up a name for the file."

	| internalStream |
	internalStream := (String new: 1000) writeStream.
	self fileOutMethod: selector on: internalStream.
	CodeExporter writeSourceCodeFrom: internalStream baseName: (self name , '-' , (selector copyReplaceAll: ':' with: '..')) isSt: true
]

{ #category : '*CodeExport' }
ClassDescription >> fileOutMethod: selector on: aStream [

	selector == #Comment ifTrue: [ ^ self inform: 'Sorry, cannot file out class comment in isolation.' ].
	(self includesSelector: selector) ifFalse: [ ^ self error: 'Selector ' , selector asString , ' not found' ].
	aStream
		header;
		timeStamp.
	self printMethodChunk: selector on: aStream
]

{ #category : '*CodeExport' }
ClassDescription >> fileOutOn: aFileStream [
	"File a description of the receiver on aFileStream."

	"we for now file out old style class definitions"

	aFileStream nextChunkPut: self oldPharoDefinition.
	self putCommentOnFile: aFileStream.
	self protocols do: [ :protocol | self fileOutLocalMethodsInProtocol: protocol on: aFileStream ]
]

{ #category : '*CodeExport' }
ClassDescription >> fileOutProtocolsOn: aFileStream [
	"File a description of the receiver's organization on aFileStream."

	aFileStream
		cr;
		nextPut: $!;
		nextChunkPut: self name , ' reorganize';
		cr;
		nextChunkPut: (String streamContents: [ :aStream |
					 self protocols do: [ :protocol |
							 aStream
								 nextPut: $(;
								 nextPutAll: protocol name printString.
							 protocol methodSelectors do: [ :selector |
								 aStream
									 space;
									 nextPutAll: selector ].
							 aStream
								 nextPut: $);
								 cr ] ]);
		cr
]

{ #category : '*CodeExport' }
ClassDescription >> printMethodChunk: selector on: outStream [
	"Copy the source code for the method associated with selector onto the fileStream."

	| preamble method |
	preamble := self name, ' methodsFor: ', (self protocolOfSelector: selector) name asString printString.
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

{ #category : '*CodeExport' }
ClassDescription >> putCommentOnFile: aFileStream [ 
	"Store the comment about the class onto file, aFileStream."

	| header |
	self comment isEmptyOrNil ifTrue: [ ^ self ].

	aFileStream
		cr;
		nextPut: $!.
	header := String streamContents: [ :strm |
		          strm
			          nextPutAll: self name;
			          nextPutAll: ' commentStamp: '.
		          self commentStamp storeOn: strm.
		          strm
			          nextPutAll: ' prior: ';
			          nextPutAll: '0' ].
	aFileStream nextChunkPut: header.
	self comment ifNotNil: [ :comment |
		aFileStream
			cr;
			nextChunkPut: comment string ].
	aFileStream cr
]
