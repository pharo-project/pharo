actions - file
loadVersionFile: fileName
	| version |
	version := self versionFromFile: fileName.
	version workingCopy repositoryGroup addRepository:
		(MCDirectoryRepository new directory: fileName asFileReference).
	version load.
