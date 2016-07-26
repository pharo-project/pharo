instance creation
validateConfigurationLoad: configurationClass version: versionString loads: loadList
	^ ((self new)
		configurationClass: configurationClass;
		yourself) validateProjectVersionLoad: versionString loads: loadList