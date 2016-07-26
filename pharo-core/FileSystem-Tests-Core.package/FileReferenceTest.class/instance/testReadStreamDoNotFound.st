tests stream
testReadStreamDoNotFound
	| ref |
	ref := filesystem / 'plonk'.
	self
		should: [ref readStreamDo: [:s]]
		raise: FileDoesNotExist 