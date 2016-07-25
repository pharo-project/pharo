private
processFinalBuffer: buffer bitLength: bitLength
	"Process given buffer, whose length may be <= 64 bytes, accumulating the results in totalA through totalE. Also process the final padding bits and length."

	| out |
	out := ByteArray new: 64.
	out replaceFrom: 1 to: buffer size with: buffer startingAt: 1.
	buffer size < 56 ifTrue: [  "padding and length fit in last data block"
		out at: buffer size + 1 put: 128.  "trailing one bit"
		self storeLength: bitLength in: out.  "end with length"
		self processBuffer: out.
		^ self].

	"process the final data block"
	buffer size < 64 ifTrue: [
		out at: buffer size + 1 put: 128].  "trailing one bit"
	self processBuffer: out.

	"process one additional block of padding ending with the length"
	out := ByteArray new: 64.  "filled with zeros"
	buffer size = 64 ifTrue: [
		"add trailing one bit that didn't fit in final data block"
		out at: 1 put: 128].
	self storeLength: bitLength in: out.
	self processBuffer: out.
