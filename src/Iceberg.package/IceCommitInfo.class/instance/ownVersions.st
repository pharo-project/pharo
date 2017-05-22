querying
ownVersions
	^ self repository savedPackages collect: [ :package | 
		self versionFor: package ]