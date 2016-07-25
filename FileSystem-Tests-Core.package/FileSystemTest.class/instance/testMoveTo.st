tests
testMoveTo
	| base file folder |
	
	base := filesystem workingDirectory.
	
	folder := (base / 'folder') ensureCreateDirectory. 
	file := (base / 'file') ensureCreateFile.
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder') ;
		markForCleanup: (base / 'file').	
	
	file moveTo: (folder / 'newFile').
	self deny: (base / 'file') exists.
	self assert: (folder / 'newFile') exists.