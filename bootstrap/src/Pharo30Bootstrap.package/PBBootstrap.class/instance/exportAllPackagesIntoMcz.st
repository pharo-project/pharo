preparation
exportAllPackagesIntoMcz

	self mczCache directory ensureDeleteAll; ensureCreateDirectory.
	self packagesToExportAsMCZ do: [ :packageName |
		self exportIntoMcz: packageName ].