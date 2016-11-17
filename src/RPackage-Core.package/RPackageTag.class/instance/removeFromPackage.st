accessing
removeFromPackage
	self 
		assert: self isEmpty 
		description: 'Package tag is not empty'.
		
	self package basicRemoveTag: self.
	
	SystemAnnouncer uniqueInstance 
		classTagRemoved: self name fromPackage: self package