category management
createCategory: aCategoryName
	| category |
	category := self basicCreateCategory: aCategoryName.
	categories add: category.
	^ category