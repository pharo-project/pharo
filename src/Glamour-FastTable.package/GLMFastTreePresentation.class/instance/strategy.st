accessing
strategy
	^ self parameters at: 'strategy' ifAbsent: [ #default ]