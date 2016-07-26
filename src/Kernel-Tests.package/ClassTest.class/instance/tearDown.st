setup
tearDown
	self deleteClass.
	self deleteRenamedClass.
	{self unclassifiedCategory. self categoryNameForTemporaryClasses} do: [:category|
			RPackage organizer unregisterPackageNamed: category]