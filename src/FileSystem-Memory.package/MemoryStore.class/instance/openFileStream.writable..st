public
openFileStream: path writable: isWriteStream
	| entry |
	entry := self basicOpen: path writable: isWriteStream.
	^ isWriteStream 
		ifTrue: [ entry writeStream ]
		ifFalse: [ entry readStream ]