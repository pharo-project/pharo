private-hacks
addHacks
	(Smalltalk at: #SystemAnnouncer) uniqueInstance suspendAllWhile: [
		RPackageOrganizer default
			registerPackageNamed: self hacksCategoryName;
			registerPackageNamed: self extensionCategoryName.
		super addHacks ]