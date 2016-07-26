reading
next
	"Access the next readable element, fail is empty"

	^ self availableForReading > 0
		ifTrue: [
			readPointer := readPointer + 1.
			buffer at: readPointer ]
		ifFalse: [
			self bufferEmptyError ]