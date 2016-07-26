snapshotting
commitToPublicRepos: aVersion

	self publicRepoLocations do: [ :loc |
		self mainRepo copy
			location: loc;
			storeVersion: aVersion ]