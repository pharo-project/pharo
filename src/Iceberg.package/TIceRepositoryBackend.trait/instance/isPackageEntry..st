private
isPackageEntry: entry
	^ entry isDirectory and: [ self canReadFileNamed: entry name ]

	