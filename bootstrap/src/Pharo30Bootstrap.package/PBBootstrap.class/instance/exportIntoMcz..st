preparation
exportIntoMcz: aPackageName

	self mczCache storeVersion: (self originRepository loadVersionFromFileNamed: aPackageName).