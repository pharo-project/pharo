accessing
showCategories: aBoolean

	showCategories := aBoolean.
	self announcer announce: ( NautilusShowCategoriesChanged boolean: aBoolean )