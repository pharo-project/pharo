public - file system operations
ensureOpen
	"Ensure that the source and changes files are opened."

	files := Array
		with: PharoFilesOpener default sourcesFileOrNil
		with: PharoFilesOpener default changesFileOrNil.
			
	readOnlyQueue := SharedQueue new.