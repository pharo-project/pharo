testing
isEmptyCategoryNamed: categoryName
	| i |
	i := categoryArray indexOf: categoryName ifAbsent: [^false].
	^self isEmptyCategoryNumber: i