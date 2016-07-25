running
setUp
	super setUp.
	classFactory := EpClassFactoryForTestCase new.
	utilPackage := RPackageOrganizer default
		registerPackageNamed: classFactory packageName.
	utilWorkingCopy := MCWorkingCopy forPackage: (MCPackage named: utilPackage name).
	utilRepo := MCDictionaryRepository new.
	self snapshot.