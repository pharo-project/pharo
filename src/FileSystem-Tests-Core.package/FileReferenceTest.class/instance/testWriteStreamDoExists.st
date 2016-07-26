tests
testWriteStreamDoExists
	| ref s path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	ref
		writeStreamDo: [ :stream | 
			s := stream.
			self deny: stream isNil ].