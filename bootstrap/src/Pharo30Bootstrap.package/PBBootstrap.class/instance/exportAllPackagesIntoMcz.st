preparation
exportAllPackagesIntoMcz

	self mczCache directory ensureDeleteAll; ensureCreateDirectory.
	self originRepository allFileNames do: [ :packageName |
		self mczCache storeVersion: (self originRepository loadVersionFromFileNamed: packageName)].