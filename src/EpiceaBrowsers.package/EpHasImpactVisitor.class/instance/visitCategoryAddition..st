visitor
visitCategoryAddition: aChange

	^ (environment organization includesCategory: aChange categoryName) not