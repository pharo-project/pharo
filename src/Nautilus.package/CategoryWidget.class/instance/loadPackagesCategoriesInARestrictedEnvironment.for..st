private
loadPackagesCategoriesInARestrictedEnvironment: env for: class
	^ self 
		sortCategories: 
			((model browsedEnvironment protocolsFor: class) copyWithFirst:  AllProtocol defaultName)
		forClass: class