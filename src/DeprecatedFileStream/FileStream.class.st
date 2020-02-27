"
I'm a deprecated class. 
Since the version 5, Pharo provides a new file streams API that makes the old one based on classes like FileStream or MultiByteBinaryOrTextStream deprecated. 
Pharo 7 makes the next important steps and removes usages of the old API from the kernel.

What you should remember:
- use file references as entry points to file streams 
- DO NOT USE FileStream class
- 'file.txt' asFileReference readStream and similar methods now return an instance of ZnCharacterReadStream instead of MultiByteFileStream
- 'file.txt' asFileReference writeStream and similar methods now return an instance of ZnCharacterWriteStream instead of MultiByteFileStream
- the new API has a clearer separation between binary and text files

1. Basic Files
By default files are binary. Not buffered.

Read UTF-8 text from an existing file
Obsolete code:
FileStream readOnlyFileNamed: '1.txt' do: [ :stream | 
    stream upToEnd ].
New code:
(File named: 'name') readStream.
(File named: 'name') readStreamDo: [ :stream | … ].
'1.txt' asFileReference readStreamDo: [ :stream | 
    stream upToEnd ].

2. Encoding
To add encoding, wrap a stream with a corresponding ZnCharacterRead/WriteStream.
“Reading”
utf8Encoded := ZnCharacterReadStream on: aBinaryStream encoding: ‘utf8’.
utf16Encoded := ZnCharacterReadStream on: aBinaryStream encoding: ‘utf16’.
“Writing”
utf8Encoded := ZnCharacterWriteStream on: aBinaryStream encoding: ‘utf8’.
utf16Encoded := ZnCharacterWriteStream on: aBinaryStream encoding: ‘utf16’.

Force creation of a new file and write a UTF-8 text
Obsolete code:
FileStream forceNewFileNamed: '1.txt' do: [ :stream | stream nextPutAll: 'a ≠ b' ].
New code:
(File named: ‘name’) writeStream.
(File named: ‘name’) writeStreamDo: [ :stream | … ].
'1.txt' asFileReference ensureDelete; 
    writeStreamDo: [ :stream | stream nextPutAll: 'a ≠ b' ].

Get all content of existing UTF-8 file
Obsolete code:
(FileStream readOnlyFileNamed: '1.txt') contentsOfEntireFile.
New code:
'1.txt' asFileReference readStream upToEnd.

3. Buffering
To add buffering, wrap a stream with a corresponding ZnBufferedRead/WriteStream.
bufferedReadStream := ZnBufferedReadStream on: aStream.
bufferedWriteStream := ZnBufferedWriteStream on: aStream.
It is in general better to buffer the reading on the binary file and apply the encoding on the buffer in memory than the other way around. See
[file := Smalltalk sourcesFile fullName.
(File named: file) readStreamDo: [ :binaryFile |
(ZnCharacterReadStream on: (ZnBufferedReadStream on: binaryFile) encoding: ‘utf8’) upToEnd
]] timeToRun. “0:00:00:09.288”
[file := Smalltalk sourcesFile fullName.
(File named: file) readStreamDo: [ :binaryFile |
(ZnBufferedReadStream on: (ZnCharacterReadStream on: binaryFile encoding: ‘utf8’)) upToEnd
]] timeToRun. “0:00:00:14.189”

The MultiByteFileStream was buffered. If you create a stream using the expression
'file.txt' asFileReference readStream.
then the ZnCharacterReadStream is not created directly on top of the stream but on top of a buffered stream that uses the file stream internally.

If you create a ZnCharacterReadStream directly on the file stream, then the characters from the file are read one by one which may be about ten times slower!
ZnCharacterReadStream on: (File openForReadFileNamed: 'file.txt').

4. File System
By default, file system files are buffered and utf8 encoded to keep backwards compatibility.
‘name’ asFileReference readStreamDo: [ :bufferedUtf8Stream | … ].
‘name’ asFileReference writeStreamDo: [ :bufferedUtf8Stream | … ].
FileStream also provides access to plain binary files using the #binaryRead/WriteStream messages. Binary streams are buffered by default too.
‘name’ asFileReference binaryReadStreamDo: [ :bufferedBinaryStream | … ].
‘name’ asFileReference binaryWriteStreamDo: [ :bufferedBinaryStream | … ].
If you want a file with another encoding (to come in the PR https://github.com/pharo-project/pharo/pull/1134), you can specify it while obtaining the stream:
‘name’ asFileReference
    readStreamEncoded: ‘utf16’
    do: [ :bufferedUtf16Stream | … ].
‘name’ asFileReference
    writeStreamEncoded: ‘utf8’
    do: [ :bufferedUtf16Stream | … ].

Force creation of a new file and write binary data into it
Obsolete code:
(FileStream forceNewFileNamed: '1.bin') 
    binary;
    nextPutAll: #[1 2 3].
New code:
'1.bin' asFileReference ensureDelete; 
    binaryWriteStreamDo: [ :stream | stream nextPutAll: #[1 2 3] ].

Read binary data from an existing file
Obsolete code:
(FileStream readOnlyFileNamed: '1.bin') binary; contentsOfEntireFile.
New code:
'1.bin' asFileReference binaryReadStream upToEnd.

Force creation of a new file with a different encoding
Obsolete code:
FileStream forceNewFileNamed: '2.txt' do: [ :stream | 
    stream converter: (TextConverter newForEncoding: 'cp-1250').
    stream nextPutAll: 'Příliš žluťoučký kůň úpěl ďábelské ódy.' ].
New code:
('2.txt' asFileReference) ensureDelete;
    writeStreamEncoded: 'cp-1250' do: [ :stream |
        stream nextPutAll: 'Příliš žluťoučký kůň úpěl ďábelské ódy.' ].

Read encoded text from an existing file
Obsolete code:
FileStream readOnlyFileNamed: '2.txt' do: [ :stream | 
    stream converter: (TextConverter newForEncoding: 'cp-1250').
    stream upToEnd ].
New code:
('2.txt' asFileReference)
    readStreamEncoded: 'cp-1250' do: [ :stream |
        stream upToEnd ].

Write a UTF-8 text to STDOUT
Obsolete code:
FileStream stdout nextPutAll: 'a ≠ b'; lf.
New code:
(ZnCharacterWriteStream on: Stdio stdout)
    nextPutAll: 'a ≠ b'; lf;
    flush.

Write CP-1250 encoded text to STDOUT
Obsolete code:
FileStream stdout 
    converter: (TextConverter newForEncoding: 'cp-1250');
    nextPutAll: 'Příliš žluťoučký kůň úpěl ďábelské ódy.'; lf.
New code:
(ZnCharacterWriteStream on: Stdio stdout encoding: 'cp1250')
    nextPutAll: 'Příliš žluťoučký kůň úpěl ďábelské ódy.'; lf;
    flush.

Read a UTF-8 text from STDIN
CAUTION: Following code will stop your VM until an input on STDIN will be provided!
Obsolete code:
FileStream stdin upTo: Character lf.
New code:
(ZnCharacterReadStream on: Stdio stdin) upTo: Character lf.
Write binary data to STDOUT
obsolete code
FileStream stdout 
    binary
    nextPutAll: #[80 104 97 114 111 10 ].
New code:
Stdio stdout 
    nextPutAll: #[80 104 97 114 111 10 ].

Read binary data from STDIN
CAUTION: Following code will stop your VM until an input on STDIN will be provided!
Obsolete code:
FileStream stdin binary upTo: 10.
New code:
Stdio stdin upTo: 10.

Positionable streams
The message #position: always works on the binary level, not on the character level.
'1.txt' asFileReference readStreamDo: [ :stream | 
    stream position: 4.
    stream upToEnd ].
This will lead to an error (ZnInvalidUTF8: Illegal leading byte for UTF-8 encoding) in case of the file created above because we set the position into the middle of a UTF-8 encoded character. To be safe, you need to read the file from the beginning.
'1.txt' asFileReference readStreamDo: [ :stream |
    3 timesRepeat: [ stream next ].
    stream upToEnd.].

5. Line Ending Conventions
If you want to write files following a specific line ending convention, use the ZnNewLineWriterStream.
This stream decorator will transform any line ending (cr, lf, crlf) into a defined line ending.
By default, it chooses the platform line ending convention.
lineWriter := ZnNewLineWriterStream on: aStream.
If you want to choose another line ending convention you can do:
lineWriter forCr.
lineWriter forLf.
lineWriter forCrLf.
lineWriter forPlatformLineEnding.
-------------------------------------------
Old comment:
 
I represent a Stream that accesses a FilePage from a File. One use for my instance is to access larger ""virtual Strings"" than can be stored contiguously in main memory. I restrict the objects stored and retrieved to be Integers or Characters. An end of file pointer terminates reading; it can be extended by writing past it, or the file can be explicitly truncated.
	
To use the file system for most applications, you typically create a FileStream. This is done by sending a message to a FileDirectory (file:, oldFile:, newFile:, rename:newName:) which creates an instance of me. Accesses to the file are then done via my instance.

*** On DOS, files cannot be shortened!  ***  To overwrite a file with a shorter one, first delete the old file (FileDirectory deleteFilePath: 'Hard Disk:aFolder:dataFolder:foo') or (aFileDirectory deleteFileNamed: 'foo').  Then write your new shorter version.
"
Class {
	#name : #FileStream,
	#superclass : #ReadWriteStream,
	#classVars : [
		'Stderr',
		'Stdin',
		'StdioFiles',
		'Stdout',
		'TheStdioHandles'
	],
	#category : #'DeprecatedFileStream-Base'
}

{ #category : #'file reader services' }
FileStream class >> changesFileSuffixes [

	^#('changes')


]

{ #category : #'concrete classes' }
FileStream class >> concreteStream [
	"Who should we really direct class queries to?  "
	^ MultiByteFileStream.

]

{ #category : #utilities }
FileStream class >> convertCRtoLF: fileName [
	"Convert the given file to LF line endings. Put the result in a file with the extention '.lf'"

	| in out c justPutCR |
	in := (self readOnlyFileNamed: fileName) binary.
	out :=  (self newFileNamed: fileName, '.lf') binary.
	justPutCR := false.
	[in atEnd] whileFalse: [
		c := in next.
		c = 10
			ifTrue: [
				out nextPut: 13.
				justPutCR := true]
			ifFalse: [
				(justPutCR and: [c = 10]) ifFalse: [out nextPut: c].
				justPutCR := false]].
	in close.
	out close.

]

{ #category : #'instance creation' }
FileStream class >> detectFile: aBlock do: anotherBlock [

	^aBlock value
		ifNil: [nil]
      	ifNotNil: [:file| [anotherBlock value: file] ensure: [file close]]
]

{ #category : #'file reader services' }
FileStream class >> fileIn: fullName [
	"File in the entire contents of the file specified by the name provided"

	| ff fn |
	fullName ifNil: [^ self inform: 'Filename is nil.'].
	fn := fullName asFileReference.
	fn := (Smalltalk hasClassNamed: #GZipReadStream) 
		ifTrue: [(Smalltalk classNamed:  #GZipReadStream) uncompressedFileName: fn fullName]
		ifFalse: [fn fullName].
	ff := self readOnlyFileNamed: fn.
	ff fileIn.

]

{ #category : #'instance creation' }
FileStream class >> fileNamed: fileName [ 
	^ self concreteStream fileNamed: (self fullName: fileName)
]

{ #category : #'instance creation' }
FileStream class >> fileNamed: fileName do: aBlock [
	"Returns the result of aBlock."
	
	^ self detectFile: [ self fileNamed: fileName ] do: aBlock
]

{ #category : #stdio }
FileStream class >> flushAndVoidStdioFiles [

	StdioFiles ifNotNil: [
		StdioFiles do: [ :file |
			file ifNotNil: [ 
				file isReadOnly ifFalse: [
					[ file flush ]
						on: Error
						do: [ :ex | "care less" ] ] ] ].
	].

	self voidStdioFiles
]

{ #category : #'instance creation' }
FileStream class >> forceNewFileNamed: fileName [
 	"Create a new file with the given name, and answer a stream opened for writing on that file. If the file already exists, delete it without asking before creating the new file."

	SessionManager default currentSession isReadOnlyAccessMode ifTrue: [ ((CannotDeleteFileException fileName: fileName)
		messageText: 'Could not delete the old version of file ' , fileName) signal.
		^ NullStream new ].

	^self concreteStream forceNewFileNamed: fileName
]

{ #category : #'instance creation' }
FileStream class >> forceNewFileNamed: fileName do: aBlock [
	"Returns the result of aBlock."
	
	^ self detectFile: [ self forceNewFileNamed: fileName ] do: aBlock
]

{ #category : #'instance creation' }
FileStream class >> fullName: fileName [
	^ fileName asFileReference fullName 
]

{ #category : #'initialize-release' }
FileStream class >> initialize [
	TheStdioHandles := Array new: 3.
	SessionManager default registerSystemClassNamed: self name
]

{ #category : #'instance creation' }
FileStream class >> isAFileNamed: fName [
	"return whether a file exists with the given name"
	^self concreteStream isAFileNamed: (self fullName: fName)
]

{ #category : #'file reader services' }
FileStream class >> isChangesFileSuffix: suffix [

	^ self changesFileSuffixes includes: suffix

]

{ #category : #deprecation }
FileStream class >> isDeprecated [
	^true
]

{ #category : #'file reader services' }
FileStream class >> isSourceFileSuffix: suffix [

	^ self sourceFileSuffixes includes: suffix

]

{ #category : #'instance creation' }
FileStream class >> new [
	^ self basicNew
]

{ #category : #'instance creation' }
FileStream class >> newFileNamed: fileName [ 
	^ self concreteStream newFileNamed: (self fullName: fileName)
]

{ #category : #'instance creation' }
FileStream class >> newFileNamed: fileName do: aBlock [
	"Returns the result of aBlock."
	
	^ self detectFile: [ self newFileNamed: fileName ] do: aBlock
]

{ #category : #stdio }
FileStream class >> newForStdio [
	"This is a hook for subclasses to initialize themselves properly."

	^self new
]

{ #category : #'instance creation' }
FileStream class >> oldFileNamed: fileName [ 
	^ self concreteStream oldFileNamed: (self fullName: fileName)
]

{ #category : #'instance creation' }
FileStream class >> oldFileNamed: fileName do: aBlock [
	"Returns the result of aBlock."
	
	^ self detectFile: [ self oldFileNamed: fileName ] do: aBlock
]

{ #category : #'instance creation' }
FileStream class >> oldFileOrNoneNamed: fileName [
	"If the file exists, answer a read-only FileStream on it. If it doesn't, answer nil."

	| fullName |
	fullName := self fullName: fileName.
	^ (self concreteStream isAFileNamed: fullName)
		ifTrue: [ self concreteStream readOnlyFileNamed: fullName]
		ifFalse: [ nil].

]

{ #category : #'instance creation' }
FileStream class >> onHandle: aFileSystemHandle [
	^ self concreteStream new
		open: aFileSystemHandle fullName
		forWrite: aFileSystemHandle isWritable
]

{ #category : #'instance creation' }
FileStream class >> readOnlyFileNamed: fileName [ 
	^ self concreteStream readOnlyFileNamed: (self fullName: fileName)
]

{ #category : #'instance creation' }
FileStream class >> readOnlyFileNamed: fileName do: aBlock [
	"Open the existing file with the given name for read-only access and pass it as argument to aBlock.
	Returns the result of aBlock."
	
	^ self detectFile: [ self readOnlyFileNamed: fileName ] do: aBlock
]

{ #category : #'file reader services' }
FileStream class >> removeLineFeeds: fullName [
	| fileContents |
	fileContents := ((FileStream readOnlyFileNamed: fullName) wantsLineEndConversion: true) contentsOfEntireFile.
	(FileStream newFileNamed: fullName) 
		nextPutAll: fileContents;
		close.
]

{ #category : #'dnd requests' }
FileStream class >> requestDropStream: dropIndex [
	"Request a read-only stream for some file that was dropped onto the application"
	^self concreteStream new requestDropStream: dropIndex.
]

{ #category : #'system startup' }
FileStream class >> shutDown: isImageQuitting [
	isImageQuitting ifTrue: [ self flushAndVoidStdioFiles ]
]

{ #category : #'file reader services' }
FileStream class >> sourceFileSuffixes [

	^#('st' 'cs')


]

{ #category : #stdio }
FileStream class >> standardIOStreamNamed: moniker forWrite: forWrite [
	"Create if necessary and store default stdin, stdout and other files based on the their names"
	
	| index |
	
	"This is an ugly hack, while waiting for a real fix for windows. There  several problems with this approach, but it allow us to run tests, etc."
	Smalltalk os isWin32 
		ifTrue: [ 
			[ ^ MultiByteFileStream forceNewFileNamed: moniker asString ]
			on: CannotDeleteFileException do: [
				"HACK: if the image is opened a second time windows barks about the already opened locked file"
				^ MultiByteFileStream forceNewFileNamed: moniker asString, '_', (Random new nextInt: SmallInteger maxVal) asString   ]]. 	
	
	index := #(stdin stdout stderr) identityIndexOf: moniker.
	^((StdioFiles ifNil: [ StdioFiles := Array new: 3 ]) at: index)
		ifNil: [
			StdioFiles
				at: index 
				put: (
					(TheStdioHandles at: index)
						ifNil: [ ^self error: moniker, ' is unavailable' ]
						ifNotNil: [ :handle |
							MultiByteFileStream newForStdio
								openOnHandle: handle
								name: moniker
								forWrite: forWrite ]) ]

]

{ #category : #'system startup' }
FileStream class >> startUp: isImageStarting [
	isImageStarting
		ifFalse: [ ^ self ].
	self voidStdioFiles.
	[ TheStdioHandles := self stdioHandles ]
		on: Error
		do: [ :ex | 
			TheStdioHandles isArray
				ifFalse: [ TheStdioHandles := Array new: 3 ] ]
]

{ #category : #stdio }
FileStream class >> stderr [

	^Stderr ifNil: [ Stderr := self standardIOStreamNamed: #stderr forWrite: true ]
]

{ #category : #stdio }
FileStream class >> stdin [

	^Stdin ifNil: [
		Stdin := self standardIOStreamNamed: #stdin forWrite: false.
		Stdin
			disableReadBuffering;
			yourself ].
]

{ #category : #stdio }
FileStream class >> stdioHandles [
	<primitive: 'primitiveFileStdioHandles' module: 'FilePlugin' error: ec>
	self primitiveFailed
]

{ #category : #stdio }
FileStream class >> stdout [

	^Stdout ifNil: [ Stdout := self standardIOStreamNamed: #stdout forWrite: true ]
]

{ #category : #stdio }
FileStream class >> voidStdioFiles [
		
	Smalltalk os isWin32 ifTrue: [ 
		{Stdout . Stderr } do: [ :each | 
			[ 	each ifNotNil: [ 
					each size isZero ifTrue: [ 
						each close.
						each name asFileReference delete	 
					] 
				]
			] ifError: [ ].
		]
	].
	
	Stdin := Stdout := Stderr := StdioFiles := nil
]

{ #category : #utilities }
FileStream class >> writeSourceCodeFrom: aStream baseName: baseName isSt: stOrCsFlag [

	| extension converter fileName  |
	[ 	extension :=  stOrCsFlag ifTrue: ['.st']  ifFalse: ['.cs'].
		converter := aStream contents isAsciiString 
			ifTrue: [MacRomanTextConverter new] 
			ifFalse: [UTF8TextConverter new].
		fileName := baseName, extension.
		fileName := FileSystem disk checkName: fileName fixErrors: true.
		[FileStream newFileNamed: fileName do: [:fileStream |
			fileName := fileStream name. "in case it is changed when handling FileExistsException"
			(converter isMemberOf: UTF8TextConverter)
				ifTrue: [fileStream binary.
						UTF8TextConverter writeBOMOn: fileStream].
			fileStream
				text;
				converter: converter;
				nextPutAll: aStream contents;
				close]] on: Abort do: [:e | ^self ]
	] on: Error do: 
	[ 	:error | 
		error isResumable 
			ifTrue: [ error resumeUnchecked: error defaultAction]
			ifFalse: [ error pass ]
	].
	self inform: 'Filed out to: ', String cr, fileName.
]

{ #category : #converting }
FileStream >> asBinaryOrTextStream [
	"I can switch between binary and text data"

	^ self
]

{ #category : #modes }
FileStream >> ascii [
	"Set this file to ascii (text) mode."

	self subclassResponsibility

]

{ #category : #testing }
FileStream >> atEnd [
	"Answer true if the current position is >= the end of file position."

	self subclassResponsibility
]

{ #category : #modes }
FileStream >> binary [
	"Set this file to binary mode."

	self subclassResponsibility

]

{ #category : #'open/close' }
FileStream >> close [
	"Close this file."

	self subclassResponsibility

]

{ #category : #testing }
FileStream >> closed [
	"Answer true if this file is closed."

	self subclassResponsibility

]

{ #category : #accessing }
FileStream >> contents [
	"Return the contents of the receiver. Do not close or otherwise touch the receiver. Return data in whatever mode the receiver is in (e.g., binary or text)."
	| s savePos |
	savePos := self position.
	self position: 0.
	s := self next: self size.
	self position: savePos.
	^s
]

{ #category : #accessing }
FileStream >> contentsOfEntireFile [
	"Read all of the contents of the receiver."

	| s binary |
	self readOnly.
	binary := self isBinary.
	self reset.	"erases knowledge of whether it is binary"
	binary ifTrue: [self binary].
	s := self next: self size.
	self close.
	^s
]

{ #category : #actions }
FileStream >> delete [
	
	(File named: self name) delete
]

{ #category : #actions }
FileStream >> exists [
	
	^ (File named: self name) exists
]

{ #category : #accessing }
FileStream >> file [
	"Answer the file for the page the receiver is streaming over."

	self subclassResponsibility
]

{ #category : #ToDeprecate }
FileStream >> fileIn [
	"Guarantee that the receiver is readOnly before fileIn for efficiency and
	to eliminate remote sharing conflicts."

	self readOnly.
	CodeImporter evaluateFileStream: self.
]

{ #category : #flushing }
FileStream >> flush [
	"When writing, this flushes the write buffer the stream uses to reduce
	the number of write() system calls it makes. This should generally be
	used before #sync, but on Windows they do the same thing."

	self subclassResponsibility

]

{ #category : #accessing }
FileStream >> localName [

	^ self name asFileReference basename

]

{ #category : #printing }
FileStream >> longPrintOn: aStream [
	"Do nothing, so it will print short.  Called to print the error file.  If the error was in a file operation, we can't read the contents of that file.  Just print its name instead."

]

{ #category : #printing }
FileStream >> longPrintOn: aStream limitedTo: sizeLimit indent: indent [

	"Do nothing, so it will print short.  Called to print the error file.  If the error was in a file operation, we can't read the contents of that file.  Just print its name instead."

	aStream cr
]

{ #category : #accessing }
FileStream >> mimeTypes [
	^ self name asFileReference mimeTypes.
]

{ #category : #accessing }
FileStream >> name [
	"Answer the name of the file for the page the receiver is streaming over."

	self subclassResponsibility
]

{ #category : #accessing }
FileStream >> next [

	(position >= readLimit and: [self atEnd])
		ifTrue: [^nil]
		ifFalse: [^collection at: (position := position + 1)]
]

{ #category : #accessing }
FileStream >> next: anInteger [

	| newCollection howManyRead increment |
	newCollection := self collectionSpecies new: anInteger.
	howManyRead := 0.
	[howManyRead < anInteger] whileTrue:
		[self atEnd ifTrue:
			[(howManyRead + 1) to: anInteger do: [:i | newCollection at: i put: (self next)].
			^newCollection].
		increment := (readLimit - position) min: (anInteger - howManyRead).
		newCollection replaceFrom: (howManyRead + 1)
			to: (howManyRead := howManyRead + increment)
			with: collection
			startingAt: (position + 1).
		position := position + increment].
	^newCollection
]

{ #category : #accessing }
FileStream >> nextPut: aByte [
	"1/31/96 sw: subclassResponsibility"

	self subclassResponsibility
]

{ #category : #accessing }
FileStream >> nextPutAll: aCollection [
	"1/31/96 sw: made subclass responsibility"

	self subclassResponsibility
]

{ #category : #positioning }
FileStream >> position [
	"Answer the current character position in the file."

	self subclassResponsibility
]

{ #category : #positioning }
FileStream >> position: pos [
	"Set the current character position in the file to pos."

	self subclassResponsibility
]

{ #category : #printing }
FileStream >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: ' on '.
	self file printOn: aStream
]

{ #category : #modes }
FileStream >> readOnly [
	"Set this file's mode to read-only."

	self subclassResponsibility

]

{ #category : #modes }
FileStream >> readOnlyStream [
	^self readOnly
]

{ #category : #modes }
FileStream >> readWrite [
	"Set this file's mode to read-write."

	self subclassResponsibility

]

{ #category : #'open/close' }
FileStream >> reopen [
	"Ensure that the receiver is open, re-open it if necessary."
	"Details: Files that were open when a snapshot occurs are no longer valid when the snapshot is resumed. This operation re-opens the file if that has happened."

	self subclassResponsibility

]

{ #category : #initialization }
FileStream >> reset [
	"Set the current character position to the beginning of the file."

	self subclassResponsibility
]

{ #category : #positioning }
FileStream >> setToEnd [
	"Set the current character position to the end of the File. The same as
	self position: self size."

	self subclassResponsibility
]

{ #category : #accessing }
FileStream >> size [
	"Answer the size of the file in characters."

	self subclassResponsibility
]

{ #category : #positioning }
FileStream >> skip: n [
	"Set the character position to n characters from the current position.
	Error if not enough characters left in the file."
 
	self subclassResponsibility
]

{ #category : #flushing }
FileStream >> sync [
	"When writing, this syncs any written/flushed data still in the kernel
	file system buffers to disk. This should generally be used after #flush,
	but on Windows they do the same thing."

	self subclassResponsibility

]

{ #category : #modes }
FileStream >> text [
	"Set this file to text (ascii) mode."

	self ascii.

]

{ #category : #positioning }
FileStream >> truncate: pos [
	"Truncate file to pos"

	self subclassResponsibility
]
