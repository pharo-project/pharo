operations
compact
	"Move data to the front of the buffer to make more room for writing at the end"
	
	| count |
	count := self availableForReading.
	count isZero ifTrue: [ ^ self reset ].
	readPointer isZero ifTrue: [ ^ self ].
	buffer replaceFrom: 1 to: count with: buffer startingAt: readPointer + 1.
	readPointer := 0.
	writePointer := count