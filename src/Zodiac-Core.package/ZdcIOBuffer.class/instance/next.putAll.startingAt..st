writing
next: count putAll: collection startingAt: offset
	"Add count elements from collection starting at offset. Fail if there is not enough room"
	
	| writeOffset |
	count > self availableForWriting ifTrue: [ self bufferFullError ].  
	writeOffset := self freeSpaceStart.
	buffer 
		replaceFrom: writeOffset 
		to: writeOffset + count - 1 
		with: collection 
		startingAt: offset.
	self advanceWritePointer: count