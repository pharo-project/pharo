bootstrapping
loadBootstrapPackagesNamed: aCollectionOfNames

	(self bootstrapVersionsSatisfying: [ :package | aCollectionOfNames includes: package name ])
		do: [ :version | self loadPackageVersion: version ].