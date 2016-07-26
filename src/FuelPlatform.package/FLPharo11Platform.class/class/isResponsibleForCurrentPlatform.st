testing
isResponsibleForCurrentPlatform
	^ super isResponsibleForCurrentPlatform and: [
		#('Pharo1.1' 'Pharo-1.1') anySatisfy: [ :version |
			SystemVersion current version beginsWith: version ] ]