testing
includesCategory: aCategory
	^(categories includes: aCategory)
		and: [super includesCategory: aCategory]