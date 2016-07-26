write and read
writeBuffer: buffer atFilePosition: fPosition onCompletionDo: aBlock
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
