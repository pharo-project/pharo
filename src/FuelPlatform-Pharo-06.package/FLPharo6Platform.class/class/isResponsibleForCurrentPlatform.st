testing
isResponsibleForCurrentPlatform
	^ super isResponsibleForCurrentPlatform and: [
		(SystemVersion current type = 'Pharo')
			and: [ SystemVersion current major = 6 ] ]