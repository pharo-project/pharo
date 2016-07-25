visitor
visitBehaviorCategoryChange: aClassRecategorizationChange
	^ (self displayBehavior: aClassRecategorizationChange classAffected name), 
		(self displayOperation: ':'),
		(self displayCategory: aClassRecategorizationChange oldCategory), 
		(self displayOperation: '->'), 
		(self displayCategory: aClassRecategorizationChange newCategory)