accessing
shouldShowHeader
	^ self parameters at: 'shouldShowHeader' ifAbsent: [ true ]