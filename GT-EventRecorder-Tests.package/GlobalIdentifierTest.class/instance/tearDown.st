running
tearDown
	super tearDown.
	"disk := FileSystem memory."
	preferences ensureDelete.
	(FileLocator workingDirectory / self folderName) 
		ensureDeleteAllChildren; 
		ensureDelete.