running
tearDown
	super tearDown.
	classFactory cleanUp.
	RPackageOrganizer default
		unregisterPackage: utilPackage.
	MCRepositoryGroup default removeRepository: self utilityRepo.