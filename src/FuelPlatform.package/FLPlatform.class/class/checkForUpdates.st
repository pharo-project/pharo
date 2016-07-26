operations
checkForUpdates
	self getConfigurationVersion = CurrentConfigurationVersion ifTrue: [ ^ self ].
	
	Gofer it
		package: 'FuelPlatform';
		url: 'http://smalltalkhub.com/mc/Pharo/Fuel/main';
		update.
		
	self updateConfigurationVersion