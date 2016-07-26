widget category
buildCategoryWidget
	| categoriesList |
	
	categoriesList := categoryWidget buildCategoriesList.	
	
	self 
		setShortcuts: #NautilusProtocolShortcuts 
		to: categoriesList. 
	
	^categoriesList