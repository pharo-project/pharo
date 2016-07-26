accessing
advanceReadPointer: count
	"Advance the read pointer as if count elements were read"
	
	self assert: count >= 0.
	count <= self availableForReading
		ifTrue: [  
			readPointer := readPointer + count ]
		ifFalse: [
			self bufferEmptyError ]  