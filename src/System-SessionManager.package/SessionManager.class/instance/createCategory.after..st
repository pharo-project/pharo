category management
createCategory: aCategoryName after: anotherCategoryName
	| category |
	category := self basicCreateCategory: aCategoryName.	
	categories 
		add: category
		after: (self categoryNamed: anotherCategoryName).
	^ category