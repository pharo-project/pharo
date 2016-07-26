private
announceLoadStart: aString
	SystemAnnouncer uniqueInstance announce: (MCVersionLoaderStarted new
		versionLoader: self;
		label: aString; 
		yourself)