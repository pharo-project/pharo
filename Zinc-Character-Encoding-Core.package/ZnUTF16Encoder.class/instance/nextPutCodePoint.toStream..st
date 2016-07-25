converting
nextPutCodePoint: codePoint toStream: stream
	"Write the encoding for integer code point to stream"

	| leadSurrogate trailSurrogate shifted |
	(self isSurrogateCodePoint: codePoint) ifTrue: [ self errorOutsideRange ].
	codePoint <= 65535
		ifTrue: [ 
			^ self write16BitWord: codePoint toStream: stream ].
	codePoint <= self maximumUTFCode
		ifTrue: [
			shifted := codePoint - 16r10000.
			leadSurrogate := 16rD800 + (shifted // 16r400).
			trailSurrogate := 16rDC00 + (shifted \\ 16r400).
			self write16BitWord: leadSurrogate toStream: stream.
			self write16BitWord: trailSurrogate toStream: stream ]
		ifFalse: [
			self errorOutsideRange ]