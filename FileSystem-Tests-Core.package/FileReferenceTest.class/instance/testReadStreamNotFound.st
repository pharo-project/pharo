tests
testReadStreamNotFound
	| ref  |
	ref := filesystem * 'plonk'.
	self
		should: [ref readStream]
		raise: FileDoesNotExist 