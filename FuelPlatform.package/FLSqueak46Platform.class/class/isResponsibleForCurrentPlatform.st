testing
isResponsibleForCurrentPlatform
	^ super isResponsibleForCurrentPlatform and: [
		(SystemVersion current version beginsWith: 'Squeak4.6') or: [
			"See #fixSystemVersion"
			(Smalltalk imageName includesSubString: '4.6') ] ]