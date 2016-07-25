morphic ui
versionInfo
	^ versionInfo ifNil: [versionInfo := repository versionInfoFromFileNamed: selectedVersion]