instance creation
configurationClass: aClass
	^ self basicNew 
		initializeConfigurationClass: aClass;
		yourself