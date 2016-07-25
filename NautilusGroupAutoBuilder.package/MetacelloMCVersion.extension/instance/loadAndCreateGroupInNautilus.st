*NautilusGroupAutoBuilder
loadAndCreateGroupInNautilus
	self doLoadRequiredFromArray: self spec defaultPackageNames.
	LoadedPackagesGroupCreatingCodeProvider
		configurationName: spec project configuration class asString
		versionString: self spec versionString
		projectName: (spec project configuration class asString withoutPrefix: 'ConfigurationOf')