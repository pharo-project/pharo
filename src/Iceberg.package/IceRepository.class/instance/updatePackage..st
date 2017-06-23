actions
updatePackage: packageName 
	self backend pull.
	(self packageNamed: packageName) loadLatest.
