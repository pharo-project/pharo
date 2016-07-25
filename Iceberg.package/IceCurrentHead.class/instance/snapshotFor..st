querying
snapshotFor: package
	^ (repository latestVersionForPackage: package) snapshot.