snapshotting
copyVersions: versions.

	versions
		do: [ :v |
			self mainRepo storeVersion: v ]
		displayingProgress: [ :v |
			'Copying ', v info name ]