patches
nextVersionName
	^ ('{1}-{2}.{3}' format: {
		self package packageName. 
		self author. 
		self info versionNumber + 1
	})
