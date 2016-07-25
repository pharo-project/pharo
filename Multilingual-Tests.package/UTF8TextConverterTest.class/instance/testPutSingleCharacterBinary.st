testing
testPutSingleCharacterBinary
	| actual |
	"Test that the raw 8/32bit representations of Characters/WideCharacters are stored when stream is binary.
	(In the case of internal writeStreams, that its destination is a ByteArray)"
	actual := ByteArray streamContents: [ :stream |
		| converter |
		converter := UTF8TextConverter new.
		converter
			nextPut: $a
			toStream: stream.
		converter
			nextPut: (Unicode value: 16r20AC)
			toStream: stream ].
	self assert: actual = #[97 0 0 16r20 16rAC]