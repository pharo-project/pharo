tests
testEnsureDeleteNonEmptyDirectory
	| reference childReference |
	reference := filesystem / 'plonk'.	"Deletes the file if it exists"
	reference ensureCreateDirectory.
	self assert: reference exists.
	
	childReference := reference / 'child'.
	childReference ensureCreateFile.
	self assert: childReference exists.
		
	self should: [reference ensureDelete] raise: DirectoryIsNotEmpty.

	reference ensureDeleteAll.