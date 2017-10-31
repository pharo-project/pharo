private
doTestsForReading: count intoBufferWithSize: readSize startingAt: startIndex fromFileOfSize: writeSize offsetBy: startOffset
	"Do the actual assertions for read/write buffers with the given parameters
	
	If offset > 0, some elements will be fetched from buffer.
	If count > writeSize - startOffset, not all will be successfully read."
	
 	| file  filename |
	filename :=  'filestream.tst'.
	[ |writeBuffer  readBuffer bytesRead|
	writeBuffer := (ByteArray new: writeSize).
	0 to: writeSize -1 do: [:ix | writeBuffer at: ix +1 put: (ix \\ 255) + 1].

	(File named: filename)
		writeStreamDo: [ :stream | stream nextPutAll: writeBuffer ].
	file := File openForReadFileNamed: filename.
	
	readBuffer := ByteArray new: readSize.

	startOffset > 0 ifTrue: [file next: startOffset].
	bytesRead := file
		readInto: readBuffer
		startingAt: startIndex
		count: count.
	"Test the count is correct, ie. either read count, or the remaining bytes in write buffer"
	self assert: ((writeSize - startOffset) min: count) equals: bytesRead.
	
	"quick test for total not written"
	self assert: readSize - bytesRead equals: (readBuffer occurrencesOf: 0).
	
	"compare test for readStream and what was in write"
	1 to: readSize do:
		[ : n | |expected|
		expected := 
			(n between: startIndex and: startIndex + bytesRead -1)
				ifTrue: [ (writeBuffer at: startOffset + n - startIndex +1) ]
				ifFalse: [ 0 ].	
		self assert: expected equals: (readBuffer at: n)  ] ]
	ensure: [ 
		file ifNotNil: [ file close ].
		(FileSystem disk workingDirectory / filename) ensureDelete ]