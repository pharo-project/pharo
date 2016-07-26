accessing
methodRules

	^ self allRules select: [ :rule | rule class checksMethod ]