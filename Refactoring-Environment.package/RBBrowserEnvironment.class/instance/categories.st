accessing
categories
	^ self systemDictionary organization categories select: [ :each | self includesCategory: each ]