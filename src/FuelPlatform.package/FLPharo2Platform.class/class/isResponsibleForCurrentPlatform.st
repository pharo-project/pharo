testing
isResponsibleForCurrentPlatform
	^ super isResponsibleForCurrentPlatform and: [
		SystemVersion current version beginsWith: 'Pharo2' ]