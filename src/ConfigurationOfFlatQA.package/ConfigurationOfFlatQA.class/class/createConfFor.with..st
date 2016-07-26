snapshotting
createConfFor: versionName with: versions

	self
		compile: (self confSourceFor: versionName with: versions)
		classified: 'versions'