public
hashStream: aPositionableStream
	"Hash the contents of the given stream from the current position to the end using the Secure Hash Algorithm. The SHA algorithm is defined in FIPS PUB 180-1. It is also described on p. 442 of 'Applied Cryptography: Protocols, Algorithms, and Source Code in C' by Bruce Schneier, Wiley, 1996."
	"http://en.wikipedia.org/wiki/Sha1#Example_hashes" 
	"(SHA1 new hashStream: 'The quick brown fox jumps over the lazy dog' readStream) hex."
	
	| startPosition buf bitLength |
	self initializeTotals.
  	
	aPositionableStream atEnd ifTrue: [self processFinalBuffer: #() bitLength: 0].

	startPosition := aPositionableStream position.
	[aPositionableStream atEnd] whileFalse: 
		[ buf := aPositionableStream next: 64.
		(aPositionableStream atEnd not and: [buf size = 64])
			ifTrue: [self processBuffer: buf]
			ifFalse: [ bitLength := (aPositionableStream position - startPosition) * 8.
					self processFinalBuffer: buf bitLength: bitLength]].
	^ self finalHash asByteArrayOfSize: 20