accessing
loadedVersions
	^ loadedVersions ifNil: [ 
		loadedVersions := Dictionary new.
		repository loadedPackages do: [ :pkg | self versionLoaded: pkg computeLoadedVersion ].
	
		loadedVersions
 	]