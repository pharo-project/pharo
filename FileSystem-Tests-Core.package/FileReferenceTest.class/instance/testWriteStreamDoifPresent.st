tests
testWriteStreamDoifPresent
	| ref s |
	ref := filesystem / 'plonk'.
	ref 
		writeStreamDo:
			[:stream |
			s := stream.
			self deny: stream isNil ]
		ifPresent: [self signalFailure: 'The file does not exist!'].
		