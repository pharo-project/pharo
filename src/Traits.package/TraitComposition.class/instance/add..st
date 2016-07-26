accessing
add: aTraitTransformation
	self errorIfNotAddable: aTraitTransformation.
	self transformations addLast: aTraitTransformation