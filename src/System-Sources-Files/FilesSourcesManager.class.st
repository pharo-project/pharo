"
I am a sources manager saving and retrieving code in a .sources and a .changes file. 

.sources file contains the code of Pharo while the .changes file contains the code compiled in the Pharo environment.

## Storing informations

For each entities complied we will save a preambul with metadatas such as the methods protocol and the source code.
The entities will store a source pointer that is used to know if the source code is stored in the .sources or .changes and the position of the source code beginning.
Since we access the source code more than the metadata it is more effective.
The format of the exporter is Chunk format (See the package Code-Export)

## Source pointers

I maps positions in the sources file to address range 16r1000000 through 16r1FFFFFF and 16r3000000 through 16r3FFFFFF, and positions in the changes file to address range 16r2000000 through 16r2FFFFFF and 16r4000000 through 16r4FFFFFF. This permits a maximum file size of 16r2000000 (32MB) for both the sources file and the changes file.

This implementation extends the source pointer address space using bit 25 of the source pointer to identify the external sources and changes files, with the remaining high order bits treated as address extension. This limits the number of external file references to two (the traditional sources and changes files). If additional external file references are needed in the future, some higher order bits in the source pointer address space should be allocated for that purpose.

The use of bit 25 of the source pointer for file references permits backward compatibility with StandardSourceFileArray, with essentially unlimited address space expansion for the sources and changes files.

## Implementation details

In order to avoid concurency problems I use a queue to manage the accesses. 

I can have multiple modes:
- Read/Write
- Read only 
- No source at all

This will depend on the state of the sources and changes stream state. If they are nil, we consider there is no source. If they are read only, we are in read only. Else we are in read/write.
"
Class {
	#name : 'FilesSourcesManager',
	#superclass : 'SourcesManager',
	#instVars : [
		'files',
		'readOnlyQueue',
		'flushChanges',
		'exporterClass'
	],
	#category : 'System-Sources-Files',
	#package : 'System-Sources-Files'
}

{ #category : 'class initialization' }
FilesSourcesManager class >> initialize [

	self beCurrent.
	
	"This next line is to add a deprecated global variable. The deprecation happened in Pharo 14 development. We can remove it completly in the future."
	self class environment at: #SourceFiles put: SourcesManager current.
	(self class environment bindingOf: #SourceFiles) isDeprecated: true.
	self deprecatedAliases: #(SourceFileArray)
]

{ #category : 'system startup' }
FilesSourcesManager class >> shutDown: isImageClosing [

	isImageClosing ifTrue: [ self default shutDown ]
]

{ #category : 'system startup' }
FilesSourcesManager class >> startUp: resuming [

	resuming ifTrue: [ self default startUp ]
]

{ #category : 'accessing' }
FilesSourcesManager >> changesFileStream [
	"Answer the master .changes FileStream, with writing permissions."

	^ files at: 2
]

{ #category : 'accessing' }
FilesSourcesManager >> changesFileStream: aStream [

	^ files at: 2 put: aStream
]

{ #category : 'private' }
FilesSourcesManager >> closeFileArray: anArray [

	anArray
		reject: [ :file | file isNil ]
		thenDo: [ :file | file close ].

	^ anArray
]

{ #category : 'public - file system operations' }
FilesSourcesManager >> closeSourceFiles [

	| oldFiles |
	oldFiles := files.
	files := Array new: 2.
	self closeFileArray: oldFiles.
	self emptyReadStreamsQueue
]

{ #category : 'private' }
FilesSourcesManager >> commentDataPointers [
	"Retrieves the combination key to look for comments in the source file"
	^'commentStamp:' -> #commentStamp:
]

{ #category : 'public - string reading' }
FilesSourcesManager >> commentTimeStampAt: sourcePointer [
	^self timeStampAt: sourcePointer for: self commentDataPointers
]

{ #category : 'private' }
FilesSourcesManager >> createReadOnlyFiles [
	| readOnly |
	readOnly := {
		self sourcesFileStream ifNotNil: [self sourcesFileStream readOnlyCopy].
		self changesFileStream ifNotNil: [self changesFileStream readOnlyCopy] }.

	^readOnly
]

{ #category : 'logging' }
FilesSourcesManager >> deferFlushDuring: aBlock [

	"defer flushing the filestream until a block has been executed"

	flushChanges ifFalse: [ ^ aBlock value ].

	flushChanges := false.

	^ [ aBlock value ] ensure: [
		flushChanges := true.
		self flushChangesFile ]
]

{ #category : 'public - file system operations' }
FilesSourcesManager >> emptyReadStreamsQueue [

	| oldQueue |
	oldQueue := readOnlyQueue.
	readOnlyQueue := SharedQueue new.
	oldQueue flushAllSuchThat: [ :each | self closeFileArray: each. true]
]

{ #category : 'public - file system operations' }
FilesSourcesManager >> ensureFlushing [
	"Ensure that the changes file has been fully written to disk by closing and re-opening it. This makes the system more robust in the face of a power failure or hard-reboot."

	| changesFile |
	changesFile := self changesFileStream ifNil: [ ^ self ].
	changesFile isReadOnly ifFalse: [ changesFile flush ].
	changesFile close.
	changesFile tryOpen.
	changesFile setToEnd
]

{ #category : 'public - file system operations' }
FilesSourcesManager >> ensureOpen [
	"Ensure that the source and changes files are opened.
	Close them before re-opening them."

	| aSourcesFile aChangesFile |
	self closeSourceFiles.

	aSourcesFile := PharoFilesOpener default sourcesFileOrNil.
	aChangesFile := PharoFilesOpener default changesFileOrNil.

	aChangesFile ifNil: [ aChangesFile := PharoFilesOpener default changesFileOrNilReadOnly: true ].

	files := Array with: aSourcesFile with: aChangesFile.

	readOnlyQueue := SharedQueue new
]

{ #category : 'system startup' }
FilesSourcesManager >> ensureSourceFilesCreated [
	
	PharoFilesOpener default locateSources ensureSourceFileCreated.
	PharoFilesOpener default locateChanges ensureSourceFileCreated.
]

{ #category : 'accessing' }
FilesSourcesManager >> exporterClass [

	^ exporterClass ifNil: [ exporterClass := CodeExporterV100 ]
]

{ #category : 'private - accessing file streams' }
FilesSourcesManager >> fileAt: index [
	^ files at: index
]

{ #category : 'private - accessing file streams' }
FilesSourcesManager >> fileAt: index ifAbsent: aBlock [
	^ files at: index ifAbsent: aBlock
]

{ #category : 'private - sourcepointer conversion' }
FilesSourcesManager >> fileIndexFromSourcePointer: anInteger [
	"Return the index of the source file which contains the source chunk addressed by anInteger"
	"1 is sources file, 2 is changes file"

	^ (anInteger bitAnd: 1) + 1
]

{ #category : 'private - sourcepointer conversion' }
FilesSourcesManager >> filePositionFromSourcePointer: anInteger [
	
	"Return the position of the source chunk addressed by anInteger"
	"The source pointer is composed of 63 bits offset + 1 bit of file identifier in the least significant bit.
	Thus, shift by one to remove the file identifier"

	^ anInteger >> 1
]

{ #category : 'private' }
FilesSourcesManager >> finishedReading: aReadOnlyFileArray from: aQueue [

	(aQueue == readOnlyQueue and: [ aQueue size < self readOnlyQueueLimit ])
		ifTrue: [ aQueue nextPut: aReadOnlyFileArray ]
		ifFalse: [ self closeFileArray: aReadOnlyFileArray ]
]

{ #category : 'public - file system operations' }
FilesSourcesManager >> flushChangesFile [

	flushChanges ifFalse: [ ^ self ].
	self changesFileStream ifNotNil: [:stream | stream flush]
]

{ #category : 'private' }
FilesSourcesManager >> fullNameForChangesNamed: aString [

	^ ((Smalltalk imageDirectory resolve: aString) , 'changes') fullName
]

{ #category : 'public - string reading' }
FilesSourcesManager >> getPreambleFrom: aSourceFileStream at: position [
	^ aSourceFileStream getPreambleAt: position
]

{ #category : 'private' }
FilesSourcesManager >> imageMovedWithNewName: newNameWithoutSuffix [

	self isChangesFileInUse ifTrue: [
			self
				closeSourceFiles;
				saveChangesInFileNamed: (self fullNameForChangesNamed: newNameWithoutSuffix) ]
]

{ #category : 'initialization' }
FilesSourcesManager >> initialize [

	files := Array new: 2.

	readOnlyQueue := SharedQueue new.
	flushChanges := true
]

{ #category : 'testing' }
FilesSourcesManager >> isChangesFileInUse [

	^ self changesFileStream isNotNil or: [ self changesFileStream closed not ]
]

{ #category : 'testing' }
FilesSourcesManager >> isSourcesPresentInSystem [

	^ true
]

{ #category : 'public - file system operations' }
FilesSourcesManager >> logChange: aString [
	"Write the argument, aString, onto the changes file."

	self
		writeChangesDuring: [ :changesFile |
				| position |
				position := changesFile position.
				changesFile
					cr;
					cr.
				(ChunkWriteStream on: changesFile)
					flushChanges: flushChanges;
					nextPut: aString.
				position ]
		onSuccess: [ :sourcePointer | "Ignore" ]
		onFail: [ "Ignore" ]
]

{ #category : 'private' }
FilesSourcesManager >> newWriter [

	^ SourceFileWriter new
		  sourceFile: self;
		  yourself
]

{ #category : 'public - file system operations' }
FilesSourcesManager >> openSourceFiles [

	self
		ensureSourceFilesCreated;
		ensureOpen
]

{ #category : 'public - string reading' }
FilesSourcesManager >> protocolAt: sourcePointer [
	"Answer the protocol for a given method, retrieved from the sources or changes file. Answer unfound protocol if no protocol is available"

	^ self protocolAt: sourcePointer for: self sourceDataPointers
]

{ #category : 'public - string reading' }
FilesSourcesManager >> protocolAt: sourcePointer for: sourceDataPointers [
	"Answer the protocol for a given method, retrieved from the sources or changes file. Answer unfound protocol if no protocol is available."

	| preamble protocol |
	protocol := 'unfound protocol'. "this is to indicate that the tagging in the source does not use the correct format.
	We will have to fix that. For example some traits methods are wrongly tagged.
	see http://code.google.com/p/pharo/issues/detail?id=4581"
	preamble := self sourcedDataAt: sourcePointer.
	preamble = 'Trait method' ifTrue: [ ^ nil ].
	(preamble includesSubstring: sourceDataPointers key) ifTrue: [
		| stream |
		" Extract the protocol string from the method preamble.
		A Preamble is a string of the shape:
		
		SourceFileArray methodsFor: 'public - string reading' stamp: '1/14/2025 10:16' prior: 62647896"
		stream := preamble readStream.
		stream skipTo: $'.
		protocol := stream upTo: $' ].
	^ protocol
]

{ #category : 'private - copying' }
FilesSourcesManager >> readOnlyCopy [
	"Answer a read only copy of self. The caller is responsible of closing it."

	^ self species new
		sourcesFileStream: self sourcesFileStream readOnlyCopy;
		changesFileStream: self changesFileStream readOnlyCopy;
		yourself
]

{ #category : 'private - copying' }
FilesSourcesManager >> readOnlyDo: aBlock [
	"Value a block with a read only copy of self."

	| copy |
	copy := self readOnlyCopy.

	^ [ aBlock value: copy ] ensure: [ copy closeSourceFiles ]
]

{ #category : 'private' }
FilesSourcesManager >> readOnlyQueueLimit [

	"maximum number of read only file arrays to be held by the readOnlyQueue(SharedQueue)"

	^ 7
]

{ #category : 'private - accessing file streams' }
FilesSourcesManager >> readStreamAt: sourcePointer ifPresent: presentBlock ifAbsent: absentBlock [

	| index position |
	sourcePointer ifNil: [ ^ absentBlock value ].

	index := self fileIndexFromSourcePointer: sourcePointer.
	index = 0 ifTrue: [ ^ absentBlock value ].	"no source pointer for this element"

	position := self filePositionFromSourcePointer: sourcePointer.
	position = 0 ifTrue: [ ^ absentBlock value ].

	^ self
		readStreamAtFileIndex: index
		atPosition: position
		ifPresent: presentBlock
		ifAbsent: absentBlock
]

{ #category : 'private - accessing file streams' }
FilesSourcesManager >> readStreamAtFileIndex: index atPosition: position ifPresent: presentBlock ifAbsent: absentBlock [

   | queue stream rofa result file |

	"This method is really ugly.
	It uses different readers over the same file that is beeing writing.
	This can lead to buffer problems.
	This should be rethougth to a better way.

	Maybe an unification of all the accesses using the same buffer could be nice."

  	queue := readOnlyQueue.
  	rofa := queue nextOrNil ifNil: [ self createReadOnlyFiles ].

   rofa ifNil: [ ^ absentBlock value ].

   stream := rofa at: index.
	stream ifNil: [ ^ absentBlock value ].

	file := files at: index.

   position > file size ifTrue: [
		self finishedReading: rofa from: queue.
		^ absentBlock value
	].

	"If the file has been flushed, I am discarding the readonly stream's buffer.
	This is to avoid a problem where the readOnly buffer is missing data that was in
	the writer buffer."
	file ensureWrittenPosition: position.

	"I am discarding the buffer of the readonly stream, because if not is out-of-sync."
	stream discardBuffer.

   stream position: position.
   result := presentBlock value: stream.

	self finishedReading: rofa from: queue.

    ^ result
]

{ #category : 'printing' }
FilesSourcesManager >> reportString [

	^ String streamContents: [ :aStream |
		{ self sourcesFileStream. self changesFileStream }
			do: [:each | each ifNotNil: [
				aStream nextPutAll: each printString; cr ] ] ]
]

{ #category : 'initialization' }
FilesSourcesManager >> reset [

	self
		shutDown;
		startUp
]

{ #category : 'private' }
FilesSourcesManager >> saveChangesInFileNamed: aString [

	| changesFile |
	changesFile := aString asFileReference.
	changesFile parent ensureCreateDirectory.
	"Ensure that the destination file is not the same as the original one"
	FileLocator changes absolutePath = changesFile absolutePath ifFalse: [
			changesFile deleteIfAbsent: [ ].
			FileLocator changes copyTo: changesFile ]
]

{ #category : 'system startup' }
FilesSourcesManager >> shutDown [

	super shutDown.
	self closeSourceFiles
]

{ #category : 'public - string reading' }
FilesSourcesManager >> sourceCodeAt: sourcePointer [
	"Retrieves the source code for the pointer received as argument"

	^ self
		readStreamAt: sourcePointer
		ifPresent: [ :stream | (ChunkReadStream on: stream) next ]
		ifAbsent: [ nil ]
]

{ #category : 'private' }
FilesSourcesManager >> sourceDataPointers [
	"Retrieves the combination key to look for information of the receiver in the source file"

	"'Point methodsFor: ''point functions'' stamp: ''lr 7/4/2009 10:42'''."

	^ 'methodsFor:' -> #stamp:
]

{ #category : 'private - sourcepointer conversion' }
FilesSourcesManager >> sourcePointerFromFileIndex: index andPosition: position [
	"Return a sourcePointer encoding the given file index and position"

	(index = 1 or: [index = 2])
		ifFalse: [self error: 'invalid source file index'].
	position < 0 ifTrue: [self error: 'invalid source code pointer'].
	
	"we shift the position by one and set the last bit for .changes"
	^ (position << 1) bitOr: index - 1
]

{ #category : 'public - string reading' }
FilesSourcesManager >> sourcedDataAt: sourcePointer [

	^ self
		readStreamAt: sourcePointer
		ifPresent: [ :file |
			| position |
			position := self filePositionFromSourcePointer: sourcePointer.
			self getPreambleFrom: file at: (0 max: position - 3) ]
		ifAbsent: [ '' ]
]

{ #category : 'accessing' }
FilesSourcesManager >> sourcesFileStream [
	"Answer the master .sources FileStream, with writing permissions."

	^ files at: 1
]

{ #category : 'accessing' }
FilesSourcesManager >> sourcesFileStream: aStream [

	^ files at: 1 put: aStream
]

{ #category : 'system startup' }
FilesSourcesManager >> startUp [

	self
		ensureSourceFilesCreated;
		ensureOpen
]

{ #category : 'public - string reading' }
FilesSourcesManager >> timeStampAt: sourcePointer [
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available."

	^ self timeStampAt: sourcePointer for: self sourceDataPointers
]

{ #category : 'public - string reading' }
FilesSourcesManager >> timeStampAt: sourcePointer for: sourceDataPointers [
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available."

	| preamble |
	flushChanges ifFalse: [ ^ '' ].
	preamble := self sourcedDataAt: sourcePointer.
	^ self timeStampFromPreamble: preamble for: sourceDataPointers
]

{ #category : 'public - string reading' }
FilesSourcesManager >> timeStampFromPreamble: preamble for: sourceDataPointers [
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available."

	| stamp index |
	stamp := ''.
	(preamble includesSubstring: sourceDataPointers key) ifTrue: [
		| stream |
		" Extract the timestamp string from the preamble.
		This can be a method or a class comment preamble.
		A method Preamble is a string of the shape:
		
			SourceFileArray methodsFor: 'public - string reading' stamp: '1/14/2025 10:16' prior: 62647896
		
		A class comment preamble is a string of the shape
		
			SHClassWithComment commentStamp: 'anStamp' prior: 0
		
		
		Lookup for the key stamp/commentStamp:, then take the string coming after that"
		
		index := preamble findLastOccurrenceOfString: sourceDataPointers value startingAt: 1.
		index = 0 ifFalse: [
			stream :=
				ReadStream
					on: preamble
					from: index + sourceDataPointers value size
					to: preamble size.
			stream upTo: $'.
			stamp := stream upTo: $' ] ].
	^ stamp
]

{ #category : 'private' }
FilesSourcesManager >> writeChangesDuring: aBlock onSuccess: successBlock onFail: failBlock [

	| file |
	file := self changesFileStream.
	file ifNil: [ ^ failBlock value ].
	
	file writeChangesDuring: aBlock
		onSuccess: [ :sourceCodePosition | successBlock value: (self sourcePointerFromFileIndex: 2 andPosition: sourceCodePosition) ]
		onFail: failBlock.

	self flushChangesFile
]

{ #category : 'logging' }
FilesSourcesManager >> writeClassCreation: aClass [

	self newWriter writeClassDefinition: aClass definitionString
]

{ #category : 'logging' }
FilesSourcesManager >> writeClassRemoved: aClass [

	self logChange: 'Smalltalk globals removeClassNamed: #' , aClass name
]

{ #category : 'logging' }
FilesSourcesManager >> writeClassRenamed: aClass oldName: aString [

	self logChange: '(Smalltalk globals at: #' , aString , ') rename: #' , aClass name
]

{ #category : 'logging' }
FilesSourcesManager >> writeComment: aString stamp: aStamp of: aClass [

	self newWriter
		timestamp: aStamp;
		prior: (aClass commentSourcePointer ifNil: [ 0 ]);
		successBlock: [ :newSourcePointer | aClass commentSourcePointer: newSourcePointer ];
		writeComment: aString ofClass: aClass
]

{ #category : 'logging' }
FilesSourcesManager >> writeMethodRemoved: aMethod from: aBehavior [

	self logChange: aBehavior name , ' removeSelector: #' , aMethod selector asString
]

{ #category : 'logging' }
FilesSourcesManager >> writeSource: source of: aMethod withFooter: footer protocol: protocol timestamp: aTimestamp priorSourcePointer: priorSourcePointer [

	self newWriter
		metadata: footer;
		timestamp: aTimestamp;
		prior: priorSourcePointer;
		successBlock: [ :newSourcePointer | "Update with new source pointer" aMethod sourcePointer: newSourcePointer ];
		failBlock: [ "if we can not store the source pointer, we put back the property" aMethod propertyAt: #source put: source ];
		writeSource: source ofMethod: aMethod protocol: protocol
]
