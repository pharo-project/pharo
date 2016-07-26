reading
peek
	"Peek the next readable element, fail is empty"

	^ self availableForReading > 0
		ifTrue: [
			buffer at: readPointer + 1 ]
		ifFalse: [
			self bufferEmptyError ]