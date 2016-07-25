protected only for tests
organizer
	^ PackageGlobalOrganizer ifNil: [ RPackageOrganizer default ].