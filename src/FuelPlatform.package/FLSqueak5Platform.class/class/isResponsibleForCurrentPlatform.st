testing
isResponsibleForCurrentPlatform
	^ super isResponsibleForCurrentPlatform and: [
		SystemVersion current version beginsWith: 'Squeak5' ]