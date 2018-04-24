"
Provides a simple, platform-independent, file stream. I am 
   - binary
   - not buffered
   - provide no encoding/conversions

!Examples of usage

""Creating a file""
stream := (File named: 'asd.txt' asFileReference fullName) readStream.

""Accessing the stream properties""
stream position.
stream atEnd.

""Writing""
stream nextPut: 17.
stream nextPutAll: 'sdd'.

""Reading""
stream next.
stream next: 2.

""Skipping""
stream skip: 2. 

""reading up to something""
stream upTo: 23.
stream upToAnyOf: #[ 13 30 ].

""peeking""
stream peek.
" 
Class {
	#name : #AbstractBinaryFileStream,
	#superclass : #Stream,
	#instVars : [
		'file',
		'handle',
		'forWrite'
	],
	#category : #'Files-Streams'
}

{ #category : #'instance creation' }
AbstractBinaryFileStream class >> handle: aCollection file: aFile forWrite: aTrue [
	
	^ self basicNew
		handle: aCollection file: aFile forWrite: aTrue;
		yourself
]

{ #category : #testing }
AbstractBinaryFileStream >> atEnd [

	^ self primAtEnd: handle
]

{ #category : #accessing }
AbstractBinaryFileStream >> contents [
	"Answer the contents of the receiver while leaving the position unchanged.
	Fail if the receiver doesn't support positioning.
	#upToEnd provides an alternative that doesn't rely on stream positioning."

	| savedPosition contents |

	savedPosition := self position.
	self position: 0.
	contents := self upToEnd.
	self position: savedPosition.
	^contents
]

{ #category : #'character writing' }
AbstractBinaryFileStream >> cr [
	
	self nextPut: Character cr asInteger
]

{ #category : #'character writing' }
AbstractBinaryFileStream >> crlf [
	
	self nextPutAll: String crlf
]

{ #category : #accessing }
AbstractBinaryFileStream >> file [
	^ file
]

{ #category : #accessing }
AbstractBinaryFileStream >> file: aFile [ 
	
	file := aFile
]

{ #category : #flushing }
AbstractBinaryFileStream >> flush [
	"When writing, this flushes the write buffer the stream uses to reduce
	the number of write() system calls it makes. This should generally be
	used before #sync, but on Windows they do the same thing."
	
	self primFlush: handle
]

{ #category : #initialization }
AbstractBinaryFileStream >> handle: aCollection file: aFile forWrite: aBoolean [
	
	handle := aCollection.
	file := aFile.
	forWrite := aBoolean
]

{ #category : #testing }
AbstractBinaryFileStream >> isBinary [
	^ true
]

{ #category : #'character writing' }
AbstractBinaryFileStream >> lf [

	self nextPut: Character lf asInteger
]

{ #category : #accessing }
AbstractBinaryFileStream >> name [

	^ file name
]

{ #category : #accessing }
AbstractBinaryFileStream >> next [
	"Answer the next byte from this file, or nil if at the end of the file."

	^ (self next: 1) ifEmpty: [ nil ] ifNotEmpty: [ :col | col first ]
]

{ #category : #accessing }
AbstractBinaryFileStream >> next: n [
	"Return a string with the next n characters of the filestream in it."

	^ self next: n into: (ByteArray new: n)
]

{ #category : #accessing }
AbstractBinaryFileStream >> next: n into: aBuffer [
	"Return a string with the next n characters of the filestream in it."
	| readBuffer read |
	readBuffer := aBuffer.
	read := self primRead: handle into: readBuffer startingAt: 1 count: n.
	^read = n 
		ifTrue: [ readBuffer ]
		ifFalse: [ readBuffer copyFrom: 1 to: read ]
]

{ #category : #reading }
AbstractBinaryFileStream >> next: n into: aString startingAt: startIndex [
	"Read n bytes into the given string.
	Return aString or a partial copy if less than
	n elements have been read."
	|read|
	read := (self readInto: aString startingAt: startIndex count: n).
	^read = n 
		ifTrue: [ aString ]
		ifFalse: [ aString copyFrom: 1 to: startIndex + read - 1 ]	
]

{ #category : #writing }
AbstractBinaryFileStream >> next: amount putAll: aByteArray [

	forWrite ifFalse: [^ self error: 'Cannot write a read-only file'].
	self primWrite: handle from: aByteArray startingAt: 1 count: amount.
	^ aByteArray
]

{ #category : #accessing }
AbstractBinaryFileStream >> nextInto: aBuffer [
	"Return a string with the next n characters of the filestream in it."

	^ self next: aBuffer size into: aBuffer
]

{ #category : #endianess }
AbstractBinaryFileStream >> nextLittleEndianNumber: n [ 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."

	| bytes s |
	bytes := self next: n.
	s := 0.
	n to: 1 by: -1 do: [:i | s := (s bitShift: 8) bitOr: (bytes at: i)].
	^ s
]

{ #category : #endianess }
AbstractBinaryFileStream >> nextLittleEndianNumber: n put: value [
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."
	| bytes |
	bytes := ByteArray new: n.
	1 to: n do: [:i | bytes at: i put: (value digitAt: i)].
	self nextPutAll: bytes
]

{ #category : #writing }
AbstractBinaryFileStream >> nextPut: anInteger [

	^ self nextPutAll: (ByteArray with: anInteger asInteger)
]

{ #category : #writing }
AbstractBinaryFileStream >> nextPutAll: aByteArray [

	forWrite ifFalse: [^ self error: 'Cannot write a read-only file'].
	
	self primWrite: handle from: aByteArray startingAt: 1 count: aByteArray basicSize.
]

{ #category : #accessing }
AbstractBinaryFileStream >> peek [
	"Answer what would be returned if the message next were sent to the receiver. If the receiver is at the end, answer nil.  "
	self subclassResponsibility
]

{ #category : #positioning }
AbstractBinaryFileStream >> position [

	^ self primGetPosition: handle
]

{ #category : #positioning }
AbstractBinaryFileStream >> position: aPosition [
	
	self primSetPosition: handle to: aPosition
]

{ #category : #private }
AbstractBinaryFileStream >> primAtEnd: id [
	"Answer true if the file position is at the end of the file."

	<primitive: 'primitiveFileAtEnd' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #private }
AbstractBinaryFileStream >> primFlush: id [
	"On Unix, the FilePlugin uses stdio FILE* structs which maintain their
	own internal buffer to minimize write() syscalls. This flushes that buffer.
	On Windows this and primSync: do the same thing."

	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>
	
	"We can't ignore fflush() failing, because it can fail for any of the
	reasons write() can."
	self primitiveFailed
]

{ #category : #private }
AbstractBinaryFileStream >> primGetPosition: id [
	"Get this files current position."

	<primitive: 'primitiveFileGetPosition' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #private }
AbstractBinaryFileStream >> primRead: id into: byteArray startingAt: startIndex count: count [
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read."

	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	self closed ifTrue: [^ self error: 'File is closed'].
	self error: 'File read failed'.

]

{ #category : #private }
AbstractBinaryFileStream >> primSetPosition: id to: anInteger [
	"Set this file to the given position."

	<primitive: 'primitiveFileSetPosition' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #private }
AbstractBinaryFileStream >> primSize: id [
	"Answer the size of this file."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #private }
AbstractBinaryFileStream >> primSizeNoError: id [
	"Answer the size of this file. Answer nil if the primitive fails; this indicates that the file handle has become stale."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	^ nil

]

{ #category : #private }
AbstractBinaryFileStream >> primSync: id [
	"On Unix, this syncs any written or flushed data still in the kernel file
	system buffers to disk. On Windows this and primFlush: do the same thing"

	<primitive: 'primitiveFileSync' module: 'FilePlugin'>

	"fsync() failing cannot be ignored"
	self primitiveFailed
]

{ #category : #private }
AbstractBinaryFileStream >> primTruncate: id to: anInteger [
	"Truncate this file to the given position."

	<primitive: 'primitiveFileTruncate' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #private }
AbstractBinaryFileStream >> primWrite: id from: stringOrByteArray startingAt: startIndex count: count [
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	(FileWriteError fileName: self name)
		signal: (self closed
			ifTrue: [ 'File ', self name, ' is closed' ]
			ifFalse: [ 'File ', self name, ' write failed' ])
]

{ #category : #printing }
AbstractBinaryFileStream >> printOn: aStream [
	"Put a printed version of the receiver onto aStream."

	aStream
		nextPutAll: self class name;
		nextPutAll: ': ';
		print: file name
]

{ #category : #reading }
AbstractBinaryFileStream >> readInto: readBuffer startingAt: startIndex count: count [

	^ self primRead: handle into: readBuffer startingAt: startIndex count: count
]

{ #category : #positioning }
AbstractBinaryFileStream >> reset [
	self position: 0
]

{ #category : #positioning }
AbstractBinaryFileStream >> setToEnd [
	
	self position: self size
]

{ #category : #accessing }
AbstractBinaryFileStream >> size [

	^ self primSize: handle
]

{ #category : #positioning }
AbstractBinaryFileStream >> skip: n [
	"Set the character position to n characters from the current position.
	Error if not enough characters left in the file.
	By default we read n characters and we avoid reading the output"
	self next: n
]

{ #category : #flushing }
AbstractBinaryFileStream >> sync [	
	"When writing, this syncs any written/flushed data still in the kernel
	file system buffers to disk. This should generally be used after #flush,
	but on Windows they do the same thing."

	self primSync: handle
]

{ #category : #accessing }
AbstractBinaryFileStream >> upTo: delim [

	^ self upToAnyOf: (ByteArray with: delim)
]

{ #category : #accessing }
AbstractBinaryFileStream >> upToAnyOf: delimiters [

	^ ByteArray new: 1000 streamContents: [ :stream | | ch |
		[ (ch := self next) isNil or: [ delimiters includes: ch] ] 
			whileFalse: [ stream nextPut: ch ] ]
]

{ #category : #accessing }
AbstractBinaryFileStream >> upToEnd [
	"Answer a subcollection from the current access position through the last element of the receiver."

	^ByteArray streamContents: [ :newStream |
		| next |
		[ (next := self next) isNil ] whileFalse: [
			newStream nextPut: next ] ]
]
