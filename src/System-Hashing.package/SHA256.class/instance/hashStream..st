accessing
hashStream: aPositionableStream
	"Hash the contents of the given stream from the current position to the end using SHA-256, the 256-bit variant of SHA-2. The SHA-2 family of algorithms are defined in FIPS PUB 180-4."
	"SHA256 new hashStream: (ReadStream on: 'foo')"

	| startPosition buf bitLength |
	self initializeTotals.

	aPositionableStream atEnd ifTrue: [ self processFinalBuffer: #() bitLength: 0 ].

	startPosition := aPositionableStream position.
	[ aPositionableStream atEnd ] whileFalse: [
		buf := aPositionableStream next: 64 into: (ByteArray new: 64).
		(aPositionableStream atEnd not and: [ buf size = 64 ])
			ifTrue: [ self processBuffer: buf ]
			ifFalse: [
				bitLength := (aPositionableStream position - startPosition) * 8.
				self processFinalBuffer: buf bitLength: bitLength ] ].

	^self finalHash asByteArrayOfSize: 32