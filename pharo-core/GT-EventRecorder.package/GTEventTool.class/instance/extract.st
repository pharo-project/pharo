api
extract
	(ZipArchive new readFrom: self defaultDownloadFilename) extractAllTo: FileLocator workingDirectory.