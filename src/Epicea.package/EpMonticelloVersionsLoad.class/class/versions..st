instance creation
versions: aCollectionOfVersions 

	^ self versionNames: (aCollectionOfVersions collect: [ :version | version info name ])