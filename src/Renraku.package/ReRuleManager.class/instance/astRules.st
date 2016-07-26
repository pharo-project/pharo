accessing
astRules
	
	^ self allRules select: [ :rule | rule class checksAST ]