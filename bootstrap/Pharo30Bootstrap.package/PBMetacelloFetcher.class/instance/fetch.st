actions
fetch
	
	| definition |
	definition := (FiMetacelloWriter new
		written: (ConfigurationOfPharoBootstrap project version: version)
		group: group).
		
	^ self patchDefinition: definition