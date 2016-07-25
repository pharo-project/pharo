accessing
packageDescriptionsFromReadableFileNames
	^ (self readableFileNames
		select: [ :packageEntry | self fileUtils directoryExists: (self fileDirectoryOn: packageEntry) ])
			collect: [ :packageEntry | self packageDescriptionFromPackageEntry: packageEntry ]