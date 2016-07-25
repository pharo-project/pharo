accessing
defaultDirectoryName
	^ defaultDirectoryName ifNil: [ defaultDirectoryName := FileLocator workingDirectory ]