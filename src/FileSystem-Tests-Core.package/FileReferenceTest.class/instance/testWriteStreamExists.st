tests
testWriteStreamExists
	| ref stream path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[stream := ref writeStream ] 
		ensure: [ stream ifNotNil: [ stream close ] ]