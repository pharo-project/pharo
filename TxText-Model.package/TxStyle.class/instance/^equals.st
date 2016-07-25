comparing
= anAttributes
	^ super = anAttributes or: [(anAttributes class == self class) and: [ attributes = anAttributes attributes ]]
