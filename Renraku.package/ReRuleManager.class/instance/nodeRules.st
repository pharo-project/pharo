accessing
nodeRules
	
	^ self allRules select: [ :rule | rule class checksNode ]