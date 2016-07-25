private - 3 installing
installNewChangesFile
	
	SourceFiles changesFileStream close.
	self updateQuitPosition.
	stream flush close.
	
	self backupOldChanges.
	
	self originalFile ensureDelete.
	newChangesFile moveTo: self originalFile.
	
	Smalltalk openSourceFiles.