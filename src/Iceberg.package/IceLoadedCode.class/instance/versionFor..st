querying
versionFor: package 
	^ self loadedVersions 
		at: package 
		ifPresent: #updated
		ifAbsent: [ self loadedVersions at: package put: package computeLoadedVersion ]
		
