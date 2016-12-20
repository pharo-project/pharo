versions
bootstrapVersionsSatisfying: aBlock

	^ RPackageOrganizer default packages
		select: [ :package | package name ~= RPackage defaultPackageName and: [ aBlock value: package ] ]
		thenCollect: [ :rpackage | self fileNameForPackageNamed: rpackage name ]