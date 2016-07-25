private - preferences
changePreferences	
	| preferenceFile |
	
	self isOmittingPreferences
		ifTrue: [ 
			commandLine := commandLine copySubcommand.
			^ self ].
		
	preferenceFile := (self optionAt: 'preferences-file') asFileReference.
	commandLine := commandLine copySubcommand.
	
	StartupPreferencesLoader default load: { preferenceFile }.