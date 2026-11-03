Extension { #name : 'AsyncFile' }

{ #category : '*FileSystem-Core-Tests' }
AsyncFile >> test: byteCount fileName: fileName [
	"AsyncFile new test: 10000 fileName: 'testData'"

	| buf1 buf2 bytesWritten bytesRead |
	buf1 := String new: byteCount withAll: $x.
	buf2 := String new: byteCount.
	self open: fileName asFileReference fullName forWrite: true.
	self
		primWriteStart: self fileHandle
		fPosition: 0
		fromBuffer: buf1
		at: 1
		count: byteCount.
	self semaphore wait.
	bytesWritten := self primWriteResult: self fileHandle.
	self close.

	self open: fileName asFileReference fullName forWrite: false.
	self primReadStart: self fileHandle fPosition: 0 count: byteCount.
	self semaphore wait.
	bytesRead := self
		             primReadResult: self fileHandle
		             intoBuffer: buf2
		             at: 1
		             count: byteCount.
	self close.

	buf1 = buf2 ifFalse: [ self error: 'buffers do not match' ].
	^ 'wrote ' , bytesWritten printString , ' bytes; ' , 'read ' , bytesRead printString , ' bytes'
]
