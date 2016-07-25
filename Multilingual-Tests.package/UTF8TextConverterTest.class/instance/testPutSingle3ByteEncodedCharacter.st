testing
testPutSingle3ByteEncodedCharacter
	| actual |
	"Test that the utf8-encoded bytes are stored when stream is ascii.
	(In the case of internal writeStreams, that its destination is a String)"
	actual := String streamContents: [ :stream |
		| converter |
		converter := UTF8TextConverter new.
		converter
			nextPut: (Unicode value: 16r20AC)
			toStream: stream ].
	self assert: actual asByteArray = #[2r11100010 2r10000010 2r10101100]
	