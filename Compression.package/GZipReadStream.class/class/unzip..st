unzipping
unzip: fullFileName 
	"Unzip the contents of the file specified by the full path name or file reference fullFileName to the current working directory
	To unzip a .zip go to the class ZipArchive"
	
	^ self unzip: fullFileName to: FileSystem disk workingDirectory 