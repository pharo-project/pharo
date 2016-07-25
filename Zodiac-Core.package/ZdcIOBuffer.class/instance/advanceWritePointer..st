accessing
advanceWritePointer: count
	"Advance the write pointer as if count elements were written"
	
	self assert: count >= 0.
	count <= self availableForWriting 
		ifTrue: [  
			writePointer := writePointer + count ]
		ifFalse: [
			self bufferFullError ]  