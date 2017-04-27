visitor
visitCategoryRename: aChange

	^ (self includesCategory: aChange oldCategoryName)
		ifTrue: [ { aChange } ]
		ifFalse: [ #() ]