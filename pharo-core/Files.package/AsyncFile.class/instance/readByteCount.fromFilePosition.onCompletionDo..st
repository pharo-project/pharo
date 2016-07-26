write and read
readByteCount: byteCount fromFilePosition: fPosition onCompletionDo: aBlock
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
