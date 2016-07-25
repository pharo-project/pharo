private
loadPackagesCategoriesInSystemEnvironmentFor: class
	
	^ self 
		sortCategories: class organization allProtocolsNames 
		forClass: class