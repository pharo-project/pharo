accessing
version
	^ version ifNil: [version := repository versionWithInfo: selectedVersion]