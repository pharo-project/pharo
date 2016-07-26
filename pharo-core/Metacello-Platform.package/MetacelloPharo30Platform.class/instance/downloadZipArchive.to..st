github support
downloadZipArchive: url to: outputFileName
  "download zip archive from <url> into <outputFileName>"

	outputFileName asFileReference ensureDelete.
	ZnClient new
		url: url;
		downloadTo: outputFileName.
    ^ ZipArchive new 
		readFrom: outputFileName asFileReference
