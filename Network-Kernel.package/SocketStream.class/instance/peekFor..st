stream in
peekFor: aCharacterOrByte
	"Read and return next character or byte
	if it is equal to the argument.
	Otherwise return false."

	| nextObject |
	self atEnd ifTrue: [^false].
	self isInBufferEmpty ifTrue: 
		[self receiveData.
		self atEnd ifTrue: [^false]].
	nextObject := inBuffer at: lastRead + 1.
	nextObject = aCharacterOrByte ifTrue: [
		lastRead := lastRead + 1.
		^true].
	^false
