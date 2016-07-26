tests
testDelete
	"Unlike ensureDelete, delete raises an exception if the file does not exist."
	| reference |
	
	reference := filesystem workingDirectory / 'does-not-exist'.
	self deny: reference exists.
	
	self 
		should: [ reference delete ]
		raise: FileDoesNotExist.
		
		
	reference := ( filesystem workingDirectory / 'file') ensureCreateFile.
	reference delete.
	
	self deny: reference exists. 
		
