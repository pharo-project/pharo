testing
isResponsibleForCurrentPlatform
	^ super isResponsibleForCurrentPlatform and: [
		#('Pharo1' 'Pharo-1') anySatisfy: [ :version |
			SystemVersion current version beginsWith: version ] ]