global
setAsGlobalCategory: aGlobalCategory
	| category |
	category := aGlobalCategory asKmCategoryIn: self.
	(self globalCategories includes: category)
		ifFalse: [ self globalCategories add: category ]