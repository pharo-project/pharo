stream out
nextPut: object
	"Write a single byte"
	
	writeBuffer isFull ifTrue: [ self flushWriteBuffer ].
	writeBuffer nextPut: object
	