testing
includesCategory: aCategory 
	^ (environment includesCategory: aCategory)
		or: [ orEnvironment includesCategory: aCategory ]