tests
testReadStreamDoifAbsent
	| ref path s |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	ref
		readStreamDo: [ :stream | 
			self deny: stream isNil.
			s := stream ]
		ifAbsent: [ self signalFailure: 'The file exists!' ].