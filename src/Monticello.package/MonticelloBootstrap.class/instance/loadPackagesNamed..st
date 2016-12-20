bootstrapping
loadPackagesNamed: aCollectionOfNames

	aCollectionOfNames do: [ :name | | filename |
		filename := self fileNameForPackageNamed: name.
		self loadPackageVersion: filename ]