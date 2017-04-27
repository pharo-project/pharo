visitor
visitCategoryRemoval: aChange

	^ (self includesCategory: aChange categoryName)
			ifTrue: [ { aChange } ]
			ifFalse: [ #() ]