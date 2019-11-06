Extension { #name : #ClassDescription }

{ #category : #'*CodeExport' }
ClassDescription >> fileOutCategory: catName [ 

	| internalStream |
	internalStream := (String new: 1000) writeStream.
	internalStream header; timeStamp.
	self fileOutCategory: catName on: internalStream.
	internalStream trailer.

	^ CodeExporter writeSourceCodeFrom: internalStream baseName: (self name , '-' , catName) isSt: true.
]

{ #category : #'*CodeExport' }
ClassDescription >> fileOutCategory: aSymbol on: aFileStream [
	"File a description of the receiver's category, aString, onto aFileStream. If 
	moveSource, is true, then set the method source pointer to the new file position.
	Note when this method is called with moveSource=true, it is condensing the
	.sources file, and should only write one preamble per method category."

	| selectors |
	aFileStream cr.
	selectors := self selectorsToFileOutCategory: aSymbol.

	"Overridden to preserve author stamps in sources file regardless"
	selectors do: [:sel |
		self printMethodChunk: sel on: aFileStream ].
	^ self
]

{ #category : #'*CodeExport' }
ClassDescription >> fileOutChangedMessages: aSet on: aFileStream [
	"File a description of the messages of this class that have been 
	changed (i.e., are entered into the argument, aSet) onto aFileStream."

	| org |
	(org := self organization) categories
		do: [ :cat | 
			| sels |
			sels := (org listAtCategoryNamed: cat) select: [ :sel | aSet includes: sel ].
			sels do: [ :sel | self printMethodChunk: sel on: aFileStream ] ]
]

{ #category : #'*CodeExport' }
ClassDescription >> fileOutLocalMethodsInCategory: aSymbol on: aFileStream [
	"File out methods for this category."
	
	| selectors |
	aFileStream cr.
	selectors := self selectorsToFileOutCategory: aSymbol.

	selectors do: [:sel | self printMethodChunk: sel on: aFileStream ].
	^ self
]

{ #category : #'*CodeExport' }
ClassDescription >> fileOutMethod: selector [
	"Write source code of a single method on a file.  Make up a name for the file."
	
	| internalStream |
	internalStream := (String new: 1000) writeStream.
	self fileOutMethod: selector on: internalStream.
	CodeExporter writeSourceCodeFrom: internalStream baseName: (self name , '-' , (selector copyReplaceAll: ':' with: '')) isSt: true.
]

{ #category : #'*CodeExport' }
ClassDescription >> fileOutMethod: selector on: aStream [
	
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asString, ' not found'].
	aStream header; timeStamp.
	self printMethodChunk: selector on: aStream.
]

{ #category : #'*CodeExport' }
ClassDescription >> fileOutOn: aFileStream [
	"File a description of the receiver on aFileStream."

	aFileStream nextChunkPut: self definition.
	self organization putCommentOnFile: aFileStream forClass: self.
	self organization categories 
		do: [ :heading | self fileOutLocalMethodsInCategory: heading on: aFileStream ]
]

{ #category : #'*CodeExport' }
ClassDescription >> fileOutOrganizationOn: aFileStream [
	"File a description of the receiver's organization on aFileStream."

	aFileStream cr; nextPut: $!.
	aFileStream nextChunkPut: self name, ' reorganize'; cr.
	aFileStream nextChunkPut: self organization stringForFileOut ; cr
]

{ #category : #'*CodeExport' }
ClassDescription >> printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod [ 
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp, and it may have a prior source link.
	If it has a prior source link, it MUST have a stamp, even if it is empty."

"The current design is that changeStamps and prior source links are preserved in the changes file.  All fileOuts include changeStamps.  Condensing sources, however, eliminates all stamps (and links, natch)."

	aFileStream cr; nextPut: $!.
	aFileStream nextChunkPut: (String streamContents:
		[:strm |
		strm nextPutAll: self name; nextPutAll: ' methodsFor: '; print: category asString.
		(changeStamp ~~ nil and:
			[changeStamp size > 0 or: [priorMethod ~~ nil]]) ifTrue:
			[strm nextPutAll: ' stamp: '; print: changeStamp].
		priorMethod ~~ nil ifTrue:
			[strm nextPutAll: ' prior: '; print: priorMethod sourcePointer]]).
	
]

{ #category : #'*CodeExport' }
ClassDescription >> printMethodChunk: selector on: outStream [
	"Copy the source code for the method associated with selector onto the fileStream."

	| preamble method |
	preamble := self name, ' methodsFor: ', (self organization categoryOfElement: selector) asString printString.
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
ClassDescription >> selectorsToFileOutCategory: aSymbol [
	^ self organization listAtCategoryNamed: aSymbol
]
