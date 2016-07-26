instance creation
activateWith: aCommandLine 
	
	^ self new 
		commandLine: (self prepareSubcommand: aCommandLine);
		activate