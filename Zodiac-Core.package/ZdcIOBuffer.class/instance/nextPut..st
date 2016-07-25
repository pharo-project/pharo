writing
nextPut: anObject
	"Add another element. Fail if full"
	
	self availableForWriting > 0
		ifTrue: [
			writePointer := writePointer + 1 .
			buffer at: writePointer put: anObject ]
		ifFalse: [
			self bufferFullError ].
	^ anObject
