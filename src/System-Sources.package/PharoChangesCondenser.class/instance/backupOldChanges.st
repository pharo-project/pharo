private - 3 installing
backupOldChanges
	| changesFile |
	
	changesFile := self originalFile.
	changesFile moveTo: (changesFile , 'bak') asFileReference nextVersion.
	