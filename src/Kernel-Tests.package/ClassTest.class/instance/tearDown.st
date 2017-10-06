running
tearDown
	self deleteClass.
	{self unclassifiedCategory. self categoryNameForTemporaryClasses} do: [:category|
			RPackage organizer unregisterPackageNamed: category]