instance creation
configurationClass: aConfigurationClass version: aVersion
	^ self basicNew 
		initializeConfigurationClass: aConfigurationClass version: aVersion;
		yourself