groups
configurationPackages
	^ (RPackageOrganizer default packages
		select: [ :package | (package name beginsWith: 'ConfigurationOf') or: [ package name beginsWith: 'BaselineOf' ] ]) asOrderedCollection