tests
testMoveToFailingExistingDestination
	| base file folder |
	
	base := filesystem workingDirectory.
	
	folder := (base / 'folder') ensureCreateDirectory. 
	(folder / 'newFile') ensureCreateFile.
	file := (base / 'file') ensureCreateFile.
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder') ;
		markForCleanup: (base / 'file').	
	
	"Destination exists already"
	self should: [ file moveTo: (folder / 'newFile') ] raise: Error.
	self assert: (base / 'file') exists.
	self assert: (folder / 'newFile') exists.