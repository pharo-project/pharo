tests
testReadStreamDoifAbsentNot
	| ref pass |
	pass := false.
	ref := filesystem * 'plonk'.
	ref 
		readStreamDo: [:stream]
		ifAbsent: [pass := true].
	self assert: pass