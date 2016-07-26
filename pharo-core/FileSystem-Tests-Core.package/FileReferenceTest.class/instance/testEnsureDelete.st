tests
testEnsureDelete
	| reference |
	reference := filesystem / 'plonk'.	"Deletes the file if it exists"
	reference ensureCreateFile.
	self assert: reference exists.
	reference ensureDelete.
	self deny: reference exists.	"No-op if file does not exist"
	reference ensureDelete