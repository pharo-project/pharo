maintenance
rebuildRepositories
	IceRepository registry do: [ :repo | 
		repo initialize.
		repo branch: nil.
		repo commitDictionary.
		repo loadedPackages do: [ :pkg | repo versionLoaded: pkg computeLoadedVersion ] ]
