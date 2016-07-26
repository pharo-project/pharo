creation
ensureCreateFile: aFileName
	^ self 
		fileEntryAt: aFileName 
		put: (ZipFileSystemFile named: aFileName)