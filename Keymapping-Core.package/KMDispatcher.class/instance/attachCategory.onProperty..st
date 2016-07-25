building
attachCategory: aCategoryName onProperty: aProperty
	self targets add: (KMCategoryBinding 
						target: [ morph perform: aProperty ] 
						morph: morph 
						category: (KMRepository default categoryForName: aCategoryName) )