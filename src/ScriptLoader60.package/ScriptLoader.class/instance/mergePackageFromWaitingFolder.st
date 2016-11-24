elementary steps
mergePackageFromWaitingFolder
	"self defaultMCWaitingFolder allFileNames"
	"self new loadPackageFromWaitingFolder"
	
	
	self waitingFolderMCZFiles
		do: [ :name | | version |
			version := self class defaultMCWaitingFolder  versionFromFileNamed: name.
			version merge.
			]