tests
testReadStreamIfAbsent
	| ref stream path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ 
	stream := ref readStreamIfAbsent: [ self signalFailure: 'Should not reach here.' ].
	 ] 
		ensure: [ stream ifNotNil: [ stream close ] ]