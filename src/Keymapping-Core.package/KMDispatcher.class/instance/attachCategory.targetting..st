building
attachCategory: aCategory targetting: anObject
	
	|  category categoryTarget |
	category := aCategory asKmCategoryIn: KMRepository default.
	categoryTarget := category bindToObject: anObject andMorph: morph.
	self targets add: categoryTarget.