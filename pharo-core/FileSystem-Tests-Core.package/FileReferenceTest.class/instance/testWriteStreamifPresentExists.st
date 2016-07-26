tests
testWriteStreamifPresentExists
	| ref pass path |
	pass := false.
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	ref writeStreamIfPresent: [ pass := true ].
	self assert: pass