instance creation
validateConfigurationLoad: configurationClass
	^ ((self new)
		configurationClass: configurationClass;
		yourself) validateProjectLoad