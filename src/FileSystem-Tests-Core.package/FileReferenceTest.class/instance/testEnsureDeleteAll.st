tests
testEnsureDeleteAll
	| reference childReference |
	reference := filesystem / 'plonk'.	"Deletes the file if it exists"
	reference ensureCreateDirectory.
	self assert: reference exists.
	
	childReference := reference / 'child'.
	childReference ensureCreateFile.
	self assert: childReference exists.
		
	reference ensureDeleteAll.
	
	self deny: childReference exists.
	self deny: reference exists.