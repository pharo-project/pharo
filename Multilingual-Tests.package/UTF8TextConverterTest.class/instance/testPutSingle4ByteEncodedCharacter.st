testing
testPutSingle4ByteEncodedCharacter
	| actual |
	"Test that the utf8-encoded bytes are stored when stream is ascii.
	(In the case of internal writeStreams, that its destination is a String)"
	actual := String streamContents: [ :stream |
		| converter |
		converter := UTF8TextConverter new.
		converter
			nextPut: (Unicode value: 16r24B62)
			toStream: stream ].
	self assert: actual asByteArray = #[2r11110000 2r10100100 2r10101101 2r10100010]
	