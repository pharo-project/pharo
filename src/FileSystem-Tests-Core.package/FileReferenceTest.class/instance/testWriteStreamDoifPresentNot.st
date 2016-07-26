tests
testWriteStreamDoifPresentNot
	| ref pass path |
	pass := false.
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	ref writeStreamDo: [ :stream |  ] ifPresent: [ pass := true ].
	self assert: pass