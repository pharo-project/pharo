tests
testReadStreamDo
	| ref path s |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	ref
		readStreamDo: [ :stream | 
			self deny: stream isNil.
			s := stream ].