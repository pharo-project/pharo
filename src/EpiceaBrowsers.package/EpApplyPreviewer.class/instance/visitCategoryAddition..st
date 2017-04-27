visitor
visitCategoryAddition: aChange

	^ (self includesCategory: aChange categoryName)
			ifTrue: [ #() ]
			ifFalse: [ { aChange } ]