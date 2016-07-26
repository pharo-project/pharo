accessing
at: index

	^ mutex critical: [ value at: index ]