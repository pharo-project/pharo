extraction
extractVersion

	^ {
		#name -> SystemVersion current version.
		#update -> SystemVersion current highestUpdate
	} asDictionary