accessing
versionInfoFromFileNamed: aFileName
	self cache 
		at: aFileName
		ifPresent: [:version | ^ version info].
	^ self loadVersionInfoFromFileNamed: aFileName