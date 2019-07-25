"
Provides a simple, platform-independent, interface to a file system. The instance variable rwmode, inherited from class PositionableStream, here is used to hold a Boolean -- true means opened for read-write, false means opened for read-only.  2/12/96 sw

I implement a simple read buffering scheme with the variables defined in PositionableStream (which are unused in me otherwise) in the following way:
	collection	<ByteString> or <ByteArray>	This is the buffer.
	position	<Integer>	The relative position in the buffer. Greater or equal to zero.
	readLimit	<Integer>	The number of bytes buffered. Greater or equal to zero.
Read buffering is enabled with #enableReadBuffering, disabled with #disableReadBuffering and it is enabled by default. The buffer is filled when a read attempt of an unbuffered absolute position is requested, or when a negative repositioning is made (with #position: with an argument < than the current absolute position) to an absolute position which is not buffered. In the first case, the buffer is positioned to the given absolute position. In the latter case the repositioning is made to the requested absolute position minus fourth of the buffer size. This means that further small negative repositionings won't result in buffer flushing. This is really useful when filing in code.
The read buffer is flushed (#flushReadBuffer) whenever a write attempt is made.
The buffer state is valid if and only if collection is not nil and position < readLimit.
"
Class {
	#name : #StandardFileStream,
	#superclass : #FileStream,
	#instVars : [
		'name',
		'fileID',
		'buffer1',
		'rwmode'
	],
	#classVars : [
		'Registry'
	],
	#category : #'DeprecatedFileStream-Base'
}

{ #category : #'error handling' }
StandardFileStream class >> fileDoesNotExistUserHandling: fullFileName [
	| selection newName |
	selection := UIManager default
		chooseFrom:
			{('create a new file' translated).
			('choose another name' translated).
			('cancel' translated)}
		title: fullFileName asFileReference basename , ' does not exist.'.
	selection = 1
		ifTrue: [ ^ self new open: fullFileName forWrite: true ].
	selection = 2
		ifFalse: [ ^ self ].
	newName := UIManager default request: 'Enter a new file name' initialAnswer: fullFileName.
	^ self oldFileNamed: (self fullName: newName)
]

{ #category : #'error handling' }
StandardFileStream class >> fileExistsUserHandling: fullFileName [
	| file localName choice newName newFullFileName |
	
	file := fullFileName asFileReference.
	localName := file basename.
	choice := UIManager default chooseFrom:{
		'overwrite that file'.
		'choose another name'.
		'cancel'
	} title: localName, ' already exists.'.
 
	choice = 1 ifTrue: [
		file delete.
		file exists ifTrue:  [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	choice = 2 ifTrue: [
		newName := UIManager default request: 'Enter a new file name' initialAnswer: fullFileName.
		newName ifNil: [Abort signal].
		newFullFileName := self fullName: newName.
		^ self newFileNamed: newFullFileName].

	Abort signal: 'Please close this to abort file opening'
]

{ #category : #'file creation' }
StandardFileStream class >> fileNamed: fileName [
	"Open a file with the given name for reading and writing. If the name has no directory part, then the file will be created in the default directory. If the file already exists, its prior contents may be modified or replaced, but the file will not be truncated on close."

	^ self new open: (self fullName: fileName) forWrite: true

]

{ #category : #'file creation' }
StandardFileStream class >> forceNewFileNamed: fileName [ 
	"Create a new file with the given name, and answer a stream opened 
	for writing on that file. If the file already exists, delete it without 
	asking before creating the new file."
	| dir  fullName f |
	
	SessionManager default currentSession isReadOnlyAccessMode ifTrue: [ ((CannotDeleteFileException fileName: fileName)
		messageText: 'Could not delete the old version of file ' , fileName) signal.
		^ NullStream new ].
	
	fullName := self fullName: fileName.
	(self isAFileNamed: fullName)
		ifFalse: [f := self new open: fullName forWrite: true.
			^ f ifNil: ["Failed to open the file"
					(FileDoesNotExistException fileName: fullName) signal]].
			
	dir := fullName asFileReference.
	dir delete.
	dir exists ifTrue: [(CannotDeleteFileException new
			messageText: 'Could not delete the old version of file ' , fullName) signal].
		
	f := self new open: fullName forWrite: true.
	^ f ifNil: ["Failed to open the file"
			(FileDoesNotExistException fileName: fullName) signal].
]

{ #category : #'file creation' }
StandardFileStream class >> isAFileNamed: fileName [
	"Answer true if a file of the given name exists."

	| f |
	f := self new open: fileName forWrite: false.
	f ifNil: [^ false].
	f close.
	^ true

]

{ #category : #'file creation' }
StandardFileStream class >> newFileNamed: fileName [
 	"Create a new file with the given name, and answer a stream opened for writing on that file. If the file already exists, ask the user what to do."

	| fullName |
	fullName := self fullName: fileName.

	^(self isAFileNamed: fullName)
		ifTrue: ["file already exists:"
			(FileExistsException fileName: fullName fileClass: self) signal]
		ifFalse: [self new open: fullName forWrite: true]


]

{ #category : #'file creation' }
StandardFileStream class >> oldFileNamed: fileName [
	"Open an existing file with the given name for reading and writing. If the name has no directory part, then the  default directory will be assumed. If the file does not exist, an exception is signaled. If the file exists, its prior contents may be modified or replaced, but the file will not be truncated on close."

	| fullName |
	fullName := self fullName: fileName.

	^(self isAFileNamed: fullName)
		ifTrue: [self new open: fullName forWrite: true]
		ifFalse: ["File does not exist..."
			(FileDoesNotExistException fileName: fullName) signal]
]

{ #category : #'error handling' }
StandardFileStream class >> readOnlyFileDoesNotExistUserHandling: fullFileName [

	| dir files choices selection newName fileName |
	
	dir := fullFileName asFileReference.
	files := dir parent fileNames.
	fileName := dir basename.
	
	choices := fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	
	selection := UIManager default chooseFrom: choices lines: (Array with: 5)
		title: (fullFileName asFileReference basename), ' does not exist.'.
		
	selection = choices size ifTrue:["cancel" ^ nil "should we raise another exception here?"].
	selection < (choices size - 1) ifTrue: [
		newName := dir parent / (choices at: selection)].
	selection = (choices size - 1) ifTrue: [
		newName := UIManager default 
							request: 'Enter a new file name' 
							initialAnswer: fileName].
	newName = '' ifFalse: [^ self readOnlyFileNamed: (self fullName: newName)].
	^ self error: 'Could not open a file'
]

{ #category : #'file creation' }
StandardFileStream class >> readOnlyFileNamed: fileName [ 
	"Open an existing file with the given name for reading."

	| fullName f |
	fullName := self fullName: fileName.
	f := self new open: fullName forWrite: false.
	^ f ifNil: ["File does not exist..."
			((FileDoesNotExistException fileName: fullName) readOnly: true) signal].

	"StandardFileStream readOnlyFileNamed: 'kjsd.txt' "
]

{ #category : #registry }
StandardFileStream class >> register: anObject [
	^self registry add: anObject
]

{ #category : #registry }
StandardFileStream class >> registry [
	^Registry ifNil: [Registry := WeakRegistry new]

]

{ #category : #registry }
StandardFileStream class >> retryWithGC: execBlock until: testBlock forFileNamed: fullName [
	"Re-implemented to only force GC if a file with the given name exists"
	| blockValue foundIt |
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	"See if we have a file with the given name"
	foundIt := self registry keys "hold on strongly for now" 
		anySatisfy:[:file| file name sameAs: fullName].
	foundIt ifFalse:[^blockValue].
	Smalltalk garbageCollectMost.
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollect.
	^execBlock value.
]

{ #category : #registry }
StandardFileStream class >> unregister: anObject [
	^self registry remove: anObject ifAbsent:[]
]

{ #category : #finalization }
StandardFileStream >> actAsExecutor [
	super actAsExecutor.
	name := nil.
]

{ #category : #modes }
StandardFileStream >> ascii [
	"opposite of binary"
	buffer1 := String new: 1.
	collection ifNotNil: [ collection := collection asString ]
]

{ #category : #testing }
StandardFileStream >> atEnd [
	"Answer whether the receiver is at its end.  "
	
	collection ifNotNil: [
		position < readLimit ifTrue: [ ^false ] ].
	^self primAtEnd: fileID
]

{ #category : #private }
StandardFileStream >> basicNext [
	"Answer the next byte from this file, or nil if at the end of the file."
	
	| count |
	collection ifNotNil: [
		position < readLimit 
			ifFalse: [ 
				readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
				position := 0.
				readLimit = 0 ifTrue: [ ^nil ] ].
		^collection at: (position := position + 1) ].	
	count := self primRead: fileID into: buffer1 startingAt: 1 count: 1.
	count = 1
		ifTrue: [ ^buffer1 at: 1 ]
		ifFalse: [ ^nil ]
]

{ #category : #modes }
StandardFileStream >> binary [
	buffer1 := ByteArray new: 1.
	collection ifNotNil: [ collection := collection asByteArray ]
]

{ #category : #'open/close' }
StandardFileStream >> close [
	"Close this file."

	fileID ifNotNil: [
		collection ifNotNil: [
			readLimit := position := 0 ].
		self primClose: fileID.
		self unregister.
		fileID := nil].

]

{ #category : #testing }
StandardFileStream >> closed [
	"Answer true if this file is closed."

	^ fileID == nil or: [ (self primSizeNoError: fileID) == nil ]

]

{ #category : #private }
StandardFileStream >> collectionSpecies [
	"Answer the species of collection into which the receiver can stream"
	
	^buffer1 species
]

{ #category : #accessing }
StandardFileStream >> directory [
	"Return the directory containing this file."

	^ self fullName asFileReference parent

]

{ #category : #private }
StandardFileStream >> disableReadBuffering [

	collection ifNotNil: [
		position < readLimit
			ifTrue: [
				| currentPosition |
				currentPosition := self position.
				collection := readLimit := position := nil.
				self position: currentPosition ]
			ifFalse: [
				collection := readLimit := position := nil ] ]
		
]

{ #category : #private }
StandardFileStream >> enableReadBuffering [

	collection ifNil: [
		buffer1 ifNotNil: [
			collection := self collectionSpecies new: 2048 ] ].
	readLimit := position := 0
]

{ #category : #'open/close' }
StandardFileStream >> ensureOpen [
	"Make sure that this file really is open."

	self closed ifTrue: [^ self reopen].
	(self primSizeNoError: fileID) ifNotNil: [^ self].
	self reopen.

]

{ #category : #accessing }
StandardFileStream >> file [
	"Answer the object representing the receiver's file.  Need for compatibility with some calls -- check senders. "

	^ self
]

{ #category : #finalization }
StandardFileStream >> finalize [
	self primCloseNoError: fileID.
]

{ #category : #accessing }
StandardFileStream >> findString: string [
	"Fast version of #upToAll: to find a String in a file starting from the beginning.
	Returns the position and also sets the position there.
	If string is not found 0 is returned and position is unchanged."

	| pos buffer count oldPos sz |
	oldPos := self position.
	self reset.
	sz := self size.
	pos := 0.
	buffer := String new: 2000.
	[ buffer := self nextInto: buffer.
	(count := buffer findString: string) > 0
		ifTrue: ["Found the string part way into buffer"
			self position: pos.
			self next: count - 1.
			^self position ].
	pos := ((pos + 2000 - string size) min: sz).
	self position: pos.
	pos = sz] whileFalse.
	"Never found it, and hit end of file"
	self position: oldPos.
	^0
]

{ #category : #accessing }
StandardFileStream >> findStringFromEnd: string [
	"Fast version to find a String in a file starting from the end.
	Returns the position and also sets the position there.
	If string is not found 0 is returned and position is unchanged."

	| pos buffer count oldPos |
	oldPos := self position.
	self setToEnd.
	pos := self position.
	[ pos := ((pos - 2000 + string size) max: 0).  "the [+ string size] allows for the case where the end of the search string is at the beginning of the current buffer"
	self position: pos.
	buffer := self next: 2000.
	(count := buffer findString: string) > 0
		ifTrue: ["Found the string part way into buffer"
			self position: pos.
			self next: count-1.  "use next instead of position:, so that CrLfFileStream can do its magic if it is being used"
			^self position].
	pos = 0] whileFalse.
	"Never found it, and hit beginning of file"
	self position: oldPos.
	^0
]

{ #category : #flushing }
StandardFileStream >> flush [
	"When writing, this flushes the write buffer the stream uses to reduce
	the number of write() system calls it makes. This should generally be
	used before #sync, but on Windows they do the same thing."

	^ self primFlush: fileID
]

{ #category : #private }
StandardFileStream >> flushReadBuffer [

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| currentPosition |
			currentPosition := self position.
			position := readLimit := 0.
			self primSetPosition: fileID to: currentPosition ] ]
]

{ #category : #accessing }
StandardFileStream >> fullName [
	"Answer this file's full path name."

	^ name

]

{ #category : #utilities }
StandardFileStream >> insertLineFeeds [
	"(FileStream oldFileNamed: 'BBfix2.st') insertLineFeeds"
	| s crLf f |
	crLf := String crlf.
	s := self next: self size.
	self close.
	f := FileStream newFileNamed: self name.
	s linesDo: [:line | f nextPutAll: line; nextPutAll: crLf].
	f close
]

{ #category : #testing }
StandardFileStream >> isBinary [
	^ buffer1 class == ByteArray
]

{ #category : #testing }
StandardFileStream >> isDirectory [
	"Answer whether the receiver represents a directory.  For the post-transition case, uncertain what to do."
	^ false
]

{ #category : #testing }
StandardFileStream >> isReadOnly [

	^ rwmode not

]

{ #category : #accessing }
StandardFileStream >> localName [
	^ name ifNotNil: [
		(name findTokens: FileSystem disk delimiter asString) last]
]

{ #category : #accessing }
StandardFileStream >> name [
	"Answer this file's full path name."

	^ name

]

{ #category : #accessing }
StandardFileStream >> next [
	"Answer the next byte from this file, or nil if at the end of the file."

	^ self basicNext
]

{ #category : #accessing }
StandardFileStream >> next: n [
	"Return a string with the next n characters of the filestream in it."
	^ self nextInto: (self collectionSpecies new: n)
]

{ #category : #accessing }
StandardFileStream >> next: n into: aString startingAt: startIndex [
	"Read n bytes into the given string.
	Return aString or a partial copy if less than
	n elements have been read."
	|read|
	read := (self readInto: aString startingAt: startIndex count: n).
	^read = n 
		ifTrue: [ aString ]
		ifFalse: [ aString copyFrom: 1 to: startIndex + read - 1 ]	
	
]

{ #category : #accessing }
StandardFileStream >> next: anInteger putAll: aString startingAt: startIndex [
	"Store the next anInteger elements from the given collection."
	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [
		position < readLimit ifTrue: [ self flushReadBuffer ] ].	
	self primWrite: fileID from: aString startingAt: startIndex count: anInteger.
	^aString
]

{ #category : #accessing }
StandardFileStream >> nextPut: char [
	"Write the given character to this file."

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	buffer1 at: 1 put: char.
	self primWrite: fileID from: buffer1 startingAt: 1 count: 1.
	^ char

]

{ #category : #accessing }
StandardFileStream >> nextPutAll: stringOrByteArray [
	"Write all the characters of the given string to this file."

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: stringOrByteArray startingAt: 1 count: stringOrByteArray basicSize.
	^ stringOrByteArray

]

{ #category : #'open/close' }
StandardFileStream >> open [
	"For compatibility with a few existing things."

	^ self reopen
]

{ #category : #'open/close' }
StandardFileStream >> open: fileName forWrite: writeMode [ 
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	| f |
	f := fileName asVmPathName.
	
	(writeMode and: [ FileSystem disk isWritable not ]) ifTrue: [ ^ nil ].

	fileID := StandardFileStream retryWithGC:[self primOpen: f writable: writeMode] 
					until:[:id| id notNil] 
					forFileNamed: fileName.
	fileID ifNil: [^ nil].  "allows sender to detect failure"
	name := fileName.
	self register.
	rwmode := writeMode.
	buffer1 := String new: 1.
	self enableReadBuffering
	
]

{ #category : #'open/close' }
StandardFileStream >> openOnHandle: aFileID name: streamName forWrite: writeMode [
	"Initialize the file with the given handle. If writeMode is true then
	 allow writing, otherwise put the file in read-only mode.
	 N.B. Do _not_ register the stream.  We do not want it to be
	 closed implicitly (e.g. on GC).  There may be multiple instances
	 accessing the same stream.  The stream is not a file."
	fileID := aFileID.
	name := streamName.
	rwmode := writeMode.
	buffer1 := String new: 1.
	self enableReadBuffering
]

{ #category : #'open/close' }
StandardFileStream >> openReadOnly [
	"Open the receiver as a read-only file."

	^ self open: name forWrite: false
]

{ #category : #accessing }
StandardFileStream >> padToEndWith: aChar [
	"On the Mac, files do not truncate.  One can delete the old file and write a new one, but sometime deletion fails (file still open? file stale?).  This is a sad compromise.  Just let the file be the same length but pad it with a harmless character."

	| pad |
	self atEnd ifTrue: [^ self].
	pad := self isBinary 
		ifTrue: [aChar asCharacter asciiValue]	"ok for char or number"
		ifFalse: [aChar asCharacter].
	self nextPutAll: (self collectionSpecies new: ((self size - self position) min: 20000) 
							withAll: pad).
]

{ #category : #accessing }
StandardFileStream >> peek [
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	| next |
	self atEnd ifTrue: [^ nil].
	next := self basicNext.
	self position: self position - 1.
	^ next
]

{ #category : #accessing }
StandardFileStream >> peekFor: item [ 
	"Answer false and do not advance if the next element is not equal to item, or if this stream is at the end.  If the next element is equal to item, then advance over it and return true"
	| next |
	(next := self next) == nil ifTrue: [^ false].
	item = next ifTrue: [^ true].
	self skip: -1.
	^ false
]

{ #category : #accessing }
StandardFileStream >> peekLast [
	"Return that item just put at the end of the stream"

	^ buffer1 size > 0 
		ifTrue: [buffer1 last]
		ifFalse: [nil]

]

{ #category : #positioning }
StandardFileStream >> position [
	"Return the receiver's current file position."

	collection ifNotNil: [
		position < readLimit ifTrue: [
			^(self primGetPosition: fileID) - readLimit + position ] ].
	^self primGetPosition: fileID
]

{ #category : #positioning }
StandardFileStream >> position: pos [
	"Set the receiver's position as indicated."

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| newPosition |
			newPosition := pos - (self primGetPosition: fileID) + readLimit.
			newPosition < 0 ifTrue: [
					| offsetPos |
					self primSetPosition: fileID to: (offsetPos := pos - (collection size // 4) max: 0).
					readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
					position := pos - offsetPos.
					^self ].
			newPosition < readLimit 
				ifTrue: [
					position := newPosition.
					^self ]
				ifFalse: [
					readLimit := position := 0 ] ] ].
	^self primSetPosition: fileID to: pos
]

{ #category : #'private - primitives' }
StandardFileStream >> primAtEnd: id [
	"Answer true if the file position is at the end of the file."

	<primitive: 'primitiveFileAtEnd' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'private - primitives' }
StandardFileStream >> primClose: id [
	"Close this file."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'private - primitives' }
StandardFileStream >> primCloseNoError: id [
	"Close this file. Don't raise an error if the primitive fails."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>

]

{ #category : #private }
StandardFileStream >> primDropRequestFileHandle: dropIndex [
	"Primitive. Return the (read-only) file handle for some file that was just dropped onto the application.
	Fail if dropIndex is out of range or the primitive is not supported."
	<primitive: 'primitiveDropRequestFileHandle' module:'DropPlugin'>
	^nil
]

{ #category : #private }
StandardFileStream >> primDropRequestFileName: dropIndex [
	"Primitive. Return the file name for some file that was just dropped onto the application.
	Fail if dropIndex is out of range or the primitive is not supported."
	<primitive: 'primitiveDropRequestFileName' module:'DropPlugin'>
	^nil
]

{ #category : #'private - primitives' }
StandardFileStream >> primFlush: id [
	"On Unix, the FilePlugin uses stdio FILE* structs which maintain their
	own internal buffer to minimize write() syscalls. This flushes that buffer.
	On Windows this and primSync: do the same thing."

	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>
	
	"We can't ignore fflush() failing, because it can fail for any of the
	reasons write() can."
	self primitiveFailed
]

{ #category : #'private - primitives' }
StandardFileStream >> primGetPosition: id [
	"Get this files current position."

	<primitive: 'primitiveFileGetPosition' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'private - primitives' }
StandardFileStream >> primOpen: fileName writable: writableFlag [
	"Open a file of the given name, and return the file ID obtained.
	If writableFlag is true, then
		if there is none with this name, then create one
		else prepare to overwrite the existing from the beginning
	otherwise
		if the file exists, open it read-only
		else return nil"

	<primitive: 'primitiveFileOpen' module: 'FilePlugin'>
	^ nil

]

{ #category : #'private - primitives' }
StandardFileStream >> primRead: id into: byteArray startingAt: startIndex count: count [
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read."

	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	self closed ifTrue: [^ self error: 'File is closed'].
	self error: 'File read failed'.

]

{ #category : #'private - primitives' }
StandardFileStream >> primSetPosition: id to: anInteger [
	"Set this file to the given position."

	<primitive: 'primitiveFileSetPosition' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'private - primitives' }
StandardFileStream >> primSize: id [
	"Answer the size of this file."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'private - primitives' }
StandardFileStream >> primSizeNoError: id [
	"Answer the size of this file. Answer nil if the primitive fails; this indicates that the file handle has become stale."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	^ nil

]

{ #category : #'private - primitives' }
StandardFileStream >> primSync: id [
	"On Unix, this syncs any written or flushed data still in the kernel file
	system buffers to disk. On Windows this and primFlush: do the same thing"

	<primitive: 'primitiveFileSync' module: 'FilePlugin'>

	"fsync() failing cannot be ignored"
	self primitiveFailed
]

{ #category : #'private - primitives' }
StandardFileStream >> primTruncate: id to: anInteger [
	"Truncate this file to the given position."

	<primitive: 'primitiveFileTruncate' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'private - primitives' }
StandardFileStream >> primWrite: id from: stringOrByteArray startingAt: startIndex count: count [
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	(FileWriteError fileName: name)
		signal: (self closed
			ifTrue: [ 'File ', name, ' is closed' ]
			ifFalse: [ 'File ', name, ' write failed' ])
]

{ #category : #printing }
StandardFileStream >> printOn: aStream [
	"Put a printed version of the receiver onto aStream."

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name
]

{ #category : #accessing }
StandardFileStream >> readInto: byteArray startingAt: startIndex count: count [
	"Read up to n bytes into the given byteArray.
	Return the number of bytes actually read."
	
	| newN newStartIndex available|
	available := 0.
	collection 
		ifNil: [ 
			newN := count.
			newStartIndex := startIndex ]
		ifNotNil: [
			byteArray class isBytes 
				ifFalse: [ 
					position < readLimit ifTrue: [ self flushReadBuffer ].
					newN := count.
					newStartIndex := startIndex ]
				ifTrue: [
					(available := readLimit - position) > 0 
						ifTrue: [
							| bufferedCount |
							bufferedCount := count min: available.
							byteArray
								replaceFrom: startIndex
								to: startIndex + bufferedCount - 1
								with: collection
								startingAt: position + 1.
							position := position + bufferedCount.
							bufferedCount = count ifTrue: [ ^count ] ].
					newN := count - available.
					newStartIndex := startIndex + available ] ].
	^available + (self primRead: fileID into: byteArray
				startingAt: newStartIndex count: newN).

]

{ #category : #modes }
StandardFileStream >> readOnly [
	"Make this file read-only."

	rwmode := false.

]

{ #category : #copying }
StandardFileStream >> readOnlyCopy [

	^ self class readOnlyFileNamed: self name.

]

{ #category : #modes }
StandardFileStream >> readWrite [
	"Make this file writable."

	rwmode := true.

]

{ #category : #registry }
StandardFileStream >> register [
	^self class register: self
]

{ #category : #'open/close' }
StandardFileStream >> reopen [
	"Close and reopen this file. The file position is reset to zero."
	"Details: Files that were open when a snapshot occurs are no longer valid when the snapshot is resumed. This operation re-opens the file if that has happened."

	fileID ifNotNil: [
		collection ifNotNil: [
			position < readLimit ifTrue: [
				self flushReadBuffer ] ].
		self primCloseNoError: fileID ].
	self open: name forWrite: rwmode.

]

{ #category : #'dnd requests' }
StandardFileStream >> requestDropStream: dropIndex [
	"Return a read-only stream for some file the user has just dropped onto the application."

	| rawName |
	rawName := self primDropRequestFileName: dropIndex.
	name := FilePathEncoder decode: rawName.
	fileID := self primDropRequestFileHandle: dropIndex.
	fileID == nil
		ifTrue: [ ^ nil ].
	self register.
	rwmode := false.
	buffer1 := String new: 1.
	self enableReadBuffering
]

{ #category : #initialization }
StandardFileStream >> reset [
	self ensureOpen.
	self position: 0.
]

{ #category : #positioning }
StandardFileStream >> setToEnd [
	"Set the position of the receiver to the end of file."

	self position: self size
]

{ #category : #accessing }
StandardFileStream >> size [
	"Answer the size of the file in characters."

	^ self primSize: fileID
]

{ #category : #positioning }
StandardFileStream >> skip: n [
	"Set the character position to n characters from the current position.
	Error if not enough characters left in the file."

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| newPosition |
			((newPosition := position + n) >= 0 and: [ newPosition < readLimit ])
				ifTrue: [ 
					position := newPosition.
					^self ] ] ].
	self position: self position + n
]

{ #category : #flushing }
StandardFileStream >> sync [
	"When writing, this syncs any written/flushed data still in the kernel
	file system buffers to disk. This should generally be used after #flush,
	but on Windows they do the same thing."

	^ self primSync: fileID
]

{ #category : #positioning }
StandardFileStream >> truncate [
	"Truncate to zero"

	^ self truncate: 0
]

{ #category : #positioning }
StandardFileStream >> truncate: pos [
	"Truncate to this position"

	self position: pos.
	^self primTruncate: fileID to: pos
]

{ #category : #registry }
StandardFileStream >> unregister [
	^self class unregister: self
]

{ #category : #accessing }
StandardFileStream >> upTo: delim [ 
	"Fast version to speed up nextChunk"
	| pos buffer count |
	collection ifNotNil: [
		(position < readLimit and: [
			(pos := collection indexOf: delim startingAt: position + 1) <= readLimit and: [
				pos > 0 ] ]) ifTrue: [
					^collection copyFrom: position + 1 to: (position := pos) - 1 ] ].
	pos := self position.
	buffer := self next: 2000.
	(count := buffer indexOf: delim) > 0 ifTrue: 
		["Found the delimiter part way into buffer"
		self position: pos + count.
		^ buffer copyFrom: 1 to: count - 1].
	self atEnd ifTrue:
		["Never found it, and hit end of file"
		^ buffer].
	"Never found it, but there's more..."
	^ buffer , (self upTo: delim)
]

{ #category : #accessing }
StandardFileStream >> upToAnyOf: delimiters do: aBlock [
	"Fast version to speed up nextChunk"
	| pos buffer count result |
	collection ifNotNil: [
		(position < readLimit and: [
			(count := collection indexOfAnyOf: delimiters startingAt: position + 1) <= readLimit and: [
				count > 0 ] ]) ifTrue: [
					result := collection copyFrom: position + 1 to: (position := count) - 1.
					aBlock value: (collection at: count).
					^result ] ].
	pos := self position.
	buffer := self next: 2000.
	(count := buffer indexOfAnyOf: delimiters) > 0 ifTrue: 
		["Found one of the delimiters part way into buffer"
		self position: pos + count.
		aBlock value: (buffer at: count).
		^ buffer copyFrom: 1 to: count - 1].
	self atEnd ifTrue:
		["Never found it, and hit end of file"
		^ buffer].
	"Never found it, but there's more..."
	^ buffer , (self upToAnyOf: delimiters do: aBlock)
]

{ #category : #accessing }
StandardFileStream >> upToEnd [
	"Answer a subcollection from the current access position through the last element of the receiver."

	^self collectionSpecies streamContents: [ :newStream |
		| next |
		[ (next := self next) == nil ] whileFalse: [
			newStream nextPut: next ] ]
]
