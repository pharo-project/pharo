accessing
versionName
	^ ('{1}-{2}.{3}' format: {
		self packageNameFromPackageDirectory. 
		self commit compatibleUsername. 
		self versionNumber
	})