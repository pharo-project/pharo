fetching
fetch
	
	| definition |
	definition := FiCargoWriter new 
		written: loadInstructions
		sourceRepository: sourceRepositoryUrl.
		
	^ self patchDefinition: definition