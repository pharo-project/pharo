accessing
removeFromPackage
	self 
		assert: self isEmpty 
		description: 'Package tag is not empty'.
		
	self package basicRemoveTag: self