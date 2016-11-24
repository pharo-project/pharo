elementary steps
copyPackagesFromWaitingFolderToHomeRepository
	"self defaultMCWaitingFolder allFileNames"
	"self new copyPackageFromWaitingFolderToHomeRepository"
	
	
	self waitingFolderMCZFiles do: [:name | | version |
		version := self class defaultMCWaitingFolder versionFromFileNamed: name.
		self repository storeVersion: version]