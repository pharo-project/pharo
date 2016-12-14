preparation
exportAllPackagesIntoMcz

	self originRepository allFileNames do: [ :packageName |
		self mczCache storeVersion: (self originRepository loadVersionFromFileNamed: packageName)].