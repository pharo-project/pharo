visitor
visitBehaviorCategoryChange: aClassRecategorization
	| oldCategory |
	oldCategory := aClassRecategorization oldCategory.
	aClassRecategorization realClassAffected category: oldCategory.