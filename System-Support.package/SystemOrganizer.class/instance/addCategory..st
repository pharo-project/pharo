accessing
addCategory: newCategory
	(self includesCategory: newCategory) ifTrue: [ ^ self ].

	super addCategory: newCategory.

	SystemAnnouncer uniqueInstance classCategoryAdded: newCategory.