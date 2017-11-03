"
An asynchronous file allows simple file read and write operations to be performed in parallel with other processing. This is useful in multimedia applications that need to stream large amounts of sound or image data from or to a file while doing other work.

Closing the file after its use is currently required to not leak external semaphores. 

"
Class {
	#name : #AsyncFile,
	#superclass : #Object,
	#instVars : [
		'name',
		'writeable',
		'semaphore',
		'fileHandle'
	],
	#classVars : [
		'Busy',
		'ErrorCode'
	],
	#category : #Files-Core
}

{ #category : #initialization }
AsyncFile class >> initialize [
	"AsyncFile initialize"

	"Possible abnormal I/O completion results."
	Busy := -1.
	ErrorCode := -2.

]

{ #category : #open/close }
AsyncFile >> close [

	fileHandle ifNil: [^ self].  "already closed"
	self primClose: fileHandle.
	Smalltalk unregisterExternalObject: semaphore.
	semaphore := nil.
	fileHandle := nil.

]

{ #category : #accessing }
AsyncFile >> fileHandle [
	^ fileHandle
]

{ #category : #open/close }
AsyncFile >> open: fullFileName forWrite: aBoolean [
	"Open a file of the given name, and return a handle for that file. Answer the receiver if the primitive succeeds, nil otherwise.
	If openForWrite is true, then:
		if there is no existing file with this name, then create one
		else open the existing file in read-write mode
	otherwise:
		if there is an existing file with this name, then open it read-only
		else answer nil."
	"Note: if an exisiting file is opened for writing, it is NOT truncated. If truncation is desired, the file should be deleted before being opened as an asynchronous file."
	"Note: On some platforms (e.g., Mac), a file can only have one writer at a time."

	| semaIndex |
	name := fullFileName.
	writeable := aBoolean.
	semaphore := Semaphore new.
	semaIndex := Smalltalk registerExternalObject: semaphore.
	fileHandle := self primOpen: name asVmPathName forWrite: writeable semaIndex: semaIndex.
	fileHandle ifNil: [
		Smalltalk unregisterExternalObject: semaphore.
		semaphore := nil.
		^ nil].

]

{ #category : #primitives }
AsyncFile >> primClose: fHandle [
	"Close this file. Do nothing if primitive fails."

	<primitive: 'primitiveAsyncFileClose' module: 'AsynchFilePlugin'>

]

{ #category : #primitives }
AsyncFile >> primOpen: fileName forWrite: openForWrite semaIndex: semaIndex [
	"Open a file of the given name, and return a handle for that file. Answer the receiver if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveAsyncFileOpen' module: 'AsynchFilePlugin'>
	^ nil

]

{ #category : #primitives }
AsyncFile >> primReadResult: fHandle intoBuffer: buffer at: startIndex count: count [
	"Copy the result of the last read operation into the given buffer starting at the given index. The buffer may be any sort of bytes or words object, excluding CompiledMethods. Answer the number of bytes read. A negative result means:
		-1 the last operation is still in progress
		-2 the last operation encountered an error"

	<primitive: 'primitiveAsyncFileReadResult' module: 'AsynchFilePlugin'>
	self primitiveFailed

]

{ #category : #primitives }
AsyncFile >> primReadStart: fHandle fPosition: fPosition count: count [
	"Start a read operation of count bytes starting at the given offset in the given file."

	<primitive: 'primitiveAsyncFileReadStart' module: 'AsynchFilePlugin'>
	self error: 'READ THE COMMENT FOR THIS METHOD.'

"NOTE: This method will fail if there is insufficient C heap to allocate an internal buffer of the required size (the value of count).  If you are trying to read a movie file, then the buffer size will be height*width*2 bytes.  Each Squeak image retains a value to be used for this allocation, and it it initially set to 0.  If you are wish to play a 640x480 movie, you need room for a buffer of 640*480*2 = 614400 bytes.  You should execute the following...

	Smalltalk extraVMMemory 2555000.

Then save-and-quit, restart, and try to open the movie file again.  If you are using Async files in another way, find out the value of count when this failure occurs (call it NNNN), and instead of the above, execute...

	Smalltalk extraVMMemory: Smalltalk extraVMMemory + NNNN

then save-and-quit, restart, and try again.
"


]

{ #category : #primitives }
AsyncFile >> primWriteResult: fHandle [
	"Answer the number of bytes written. A negative result means:
		-1 the last operation is still in progress
		-2 the last operation encountered an error"

	<primitive: 'primitiveAsyncFileWriteResult' module: 'AsynchFilePlugin'>
	self primitiveFailed

]

{ #category : #primitives }
AsyncFile >> primWriteStart: fHandle fPosition: fPosition fromBuffer: buffer at: startIndex count: count [
	"Start a write operation of count bytes starting at the given index in the given buffer. The buffer may be any sort of bytes or words object, excluding CompiledMethods. The contents of the buffer are copied into an internal buffer immediately, so the buffer can be reused after the write operation has been started. Fail if there is insufficient C heap to allocate an internal buffer of the requested size."

	<primitive: 'primitiveAsyncFileWriteStart' module: 'AsynchFilePlugin'>
	writeable ifFalse: [^ self error: 'attempt to write a file opened read-only'].
	self primitiveFailed

]

{ #category : #'write and read' }
AsyncFile >> readByteCount: byteCount fromFilePosition: fPosition onCompletionDo: aBlock [
	"Start a read operation to read byteCount's from the given position in this file. and fork a process to await its completion. When the operation completes, evaluate the given block. Note that, since the completion block may run asynchronous, the client may need to use a SharedQueue or a semaphore for synchronization."

	| buffer |
	buffer := String new: byteCount.
	self primReadStart: fileHandle fPosition: fPosition count: byteCount.
	"here's the process that awaits the results:"
	[| n |
		[	semaphore wait.
		  	n := self primReadResult: fileHandle intoBuffer: buffer at: 1 count: byteCount.
		  	n = Busy.
		] whileTrue.  "loop while busy in case the semaphore had excess signals"
		n = ErrorCode ifTrue: [^ self error: 'asynchronous read operation failed'].
		aBlock value: buffer.
	] forkAt: Processor userInterruptPriority.

]

{ #category : #tests }
AsyncFile >> test: byteCount fileName: fileName [
	"AsyncFile new test: 10000 fileName: 'testData'"

	| buf1 buf2 bytesWritten bytesRead |
	buf1 := String new: byteCount withAll: $x.
	buf2 := String new: byteCount.
	self open: fileName  asFileReference fullName forWrite: true.
	self primWriteStart: fileHandle
		fPosition: 0
		fromBuffer: buf1
		at: 1
		count: byteCount.
	semaphore wait.
	bytesWritten := self primWriteResult: fileHandle.
	self close.
	
	self open: fileName asFileReference fullName forWrite: false.
	self primReadStart: fileHandle fPosition: 0 count: byteCount.
	semaphore wait.
	bytesRead :=
		self primReadResult: fileHandle
			intoBuffer: buf2
			at: 1
			count: byteCount.
	self close.

	buf1 = buf2 ifFalse: [self error: 'buffers do not match'].
	^ 'wrote ', bytesWritten printString, ' bytes; ',
	   'read ', bytesRead printString, ' bytes'

]

{ #category : #'write and read' }
AsyncFile >> waitForCompletion [
	semaphore wait
]

{ #category : #'write and read' }
AsyncFile >> writeBuffer: buffer atFilePosition: fPosition onCompletionDo: aBlock [
	"Start an operation to write the contents of the buffer at given position in this file, and fork a process to await its completion. When the write completes, evaluate the given block. Note that, since the completion block runs asynchronously, the client may need to use a SharedQueue or a semaphore for synchronization."

	self primWriteStart: fileHandle
		fPosition: fPosition
		fromBuffer: buffer
		at: 1
		count: buffer size.
	"here's the process that awaits the results:"
	[| n |
		[	semaphore wait.
		  	n := self primWriteResult: fileHandle.
		  	n = Busy.
		] whileTrue.  "loop while busy in case the semaphore had excess signals"
		n = ErrorCode ifTrue: [^ self error: 'asynchronous write operation failed'].
		n = buffer size ifFalse: [^ self error: 'did not write the entire buffer'].
		aBlock value.
	] forkAt: Processor userInterruptPriority.

]
