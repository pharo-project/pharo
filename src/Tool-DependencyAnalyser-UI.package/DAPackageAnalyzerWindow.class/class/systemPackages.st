examples
systemPackages
	^ RPackageOrganizer default packages collect: [ :package | package packageName asString ]