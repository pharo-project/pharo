running
tearDown
	MCWorkingCopy removeDependent: self emptyOrganizer.
	self cleanClassesPackagesAndCategories.
	super tearDown.