tests
testWriteStreamDo
	| ref s |
	ref := filesystem / 'plonk'.
	ref writeStreamDo:
		[:stream |
		s := stream.
		self deny: stream isNil ].
		