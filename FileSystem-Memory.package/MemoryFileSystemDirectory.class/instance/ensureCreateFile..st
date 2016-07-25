creation
ensureCreateFile: aFileName
	^ self 
		fileEntryAt: aFileName 
		put: (MemoryFileSystemFile named: aFileName)