testing
includesCategory: aCategory
	^ (environment includesCategory: aCategory) and: [ (self classNamesFor: aCategory) notEmpty ]