github support
extractRepositoryFrom: zipFile to: directory
    "unzip <zipFile> into <directory>"
	ZipArchive new 
		readFrom:  zipFile;
		extractAllTo: directory asFileReference.
	zipFile asFileReference delete.