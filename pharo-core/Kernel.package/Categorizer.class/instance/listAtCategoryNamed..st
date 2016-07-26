queries
listAtCategoryNamed: categoryName
	"Answer the array of elements associated with the name, categoryName."

	| i |
	i := categoryArray indexOf: categoryName ifAbsent: [^Array new].
	^self listAtCategoryNumber: i