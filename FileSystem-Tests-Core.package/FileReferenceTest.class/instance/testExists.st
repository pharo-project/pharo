tests
testExists

	| reference |
	reference := filesystem / 'plonk'.
	
	reference ensureCreateFile.
	self assert: reference exists.

	reference delete.
	self deny: reference exists. 