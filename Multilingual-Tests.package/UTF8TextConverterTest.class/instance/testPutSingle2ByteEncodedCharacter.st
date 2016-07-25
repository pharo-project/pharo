testing
testPutSingle2ByteEncodedCharacter
	| actual |
	"Test that the utf8-encoded bytes are stored when stream is ascii.
	(In the case of internal writeStreams, that its destination is a String)"
	actual := String streamContents: [ :stream |
		| converter |
		converter := UTF8TextConverter new.
		converter
			nextPut: $ç
			toStream: stream ].
	self assert: actual asByteArray = #[2r11000011  2r10100111]
	