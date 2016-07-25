private
announceLoadStop: aString
	SystemAnnouncer uniqueInstance announce: (MCVersionLoaderStopped new
		versionLoader: self;
		label: aString; 
		yourself)