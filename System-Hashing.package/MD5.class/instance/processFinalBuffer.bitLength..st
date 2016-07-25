private-buffers
processFinalBuffer: aByteArray bitLength: bitLength 
	"Pad the buffer until we have an even 64 bytes, then transform"
	| out |
	out := ByteArray new: 64.
	out 
		replaceFrom: 1
		to: aByteArray size
		with: aByteArray
		startingAt: 1.
	aByteArray size < 56 ifTrue: 
		[ out 
			at: aByteArray size + 1
			put: 128.	"trailing bit"
		self 
			storeLength: bitLength
			in: out.
		self processBuffer: out.
		^ self ].

	"not enough room for the length, so just pad this one, then..."
	aByteArray size < 64 ifTrue: 
		[ out 
			at: aByteArray size + 1
			put: 128 ].
	self processBuffer: out.

	"process one additional block of padding ending with the length"
	out := ByteArray new: 64.	"filled with zeros"
	aByteArray size = 64 ifTrue: 
		[ out 
			at: 1
			put: 128 ].
	self 
		storeLength: bitLength
		in: out.
	self processBuffer: out