snapshotting
createSymbolicFor: versionName

	self
		compile: (self symbolicSourceFor: versionName)
		classified: 'symbolic versions'