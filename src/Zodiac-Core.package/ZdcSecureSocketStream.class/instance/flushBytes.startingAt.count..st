private out
flushBytes: bytes startingAt: offset count: count
	"Ask to write count bytes starting from offset. Wait. Fail if not successful
	Overwritten: first encrypt the data, then send it to the socket using a super call."
	
	| remaining currentOffset |
	self isConnected ifFalse: [ ConnectionClosed signal: 'Cannot write data' ].  
	remaining := count.
	currentOffset := offset.
	[ remaining > 0 ] whileTrue: [ | chunkCount writeCount |
		self resetEncryptedBuffers.
		chunkCount := 4096 min: remaining.
		writeCount := self sslSession encrypt: bytes from: currentOffset to: currentOffset + chunkCount - 1 into: out.
		writeCount < 0 ifTrue: [ ^ self sslException: 'encrypt failed' code: writeCount ].
		self flushEncryptedBytes: out startingAt: 1 count: writeCount.
		remaining := remaining - chunkCount.
		currentOffset := currentOffset + chunkCount ]