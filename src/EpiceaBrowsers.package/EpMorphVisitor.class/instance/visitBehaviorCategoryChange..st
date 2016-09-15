visitor
visitBehaviorCategoryChange: aClassRecategorizationChange

	^	(self displayOperation: 'Move'),
		(self displayBehavior: aClassRecategorizationChange behaviorAffectedName), 
		(self displayOperation: 'from tag'),
		(self displayString: aClassRecategorizationChange oldCategory), 
		(self displayOperation: 'to'), 
		(self displayString: aClassRecategorizationChange newCategory)