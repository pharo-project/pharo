actions
updatePackage: packageName 
	self pull.
	(self packageNamed: packageName) loadLatest.
