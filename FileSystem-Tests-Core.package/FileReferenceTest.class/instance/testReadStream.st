tests
testReadStream
	| ref stream path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[stream := ref readStream. ] 
		ensure: [ stream ifNotNil: [ stream close ] ]