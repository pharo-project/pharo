testing
isResponsibleForCurrentPlatform
	^ super isResponsibleForCurrentPlatform and: [
		SystemVersion current version beginsWith: 'Squeak4.1' ]