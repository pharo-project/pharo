preparation
exportAllPackagesIntoMcz

	self mczCache directory ensureDeleteAll; ensureCreateDirectory.
	self originRepository allFileNames do: [ :packageName |
		self exportIntoMcz: packageName ].