accessing
basicVersion
	^ (MCVersion new)
		setPackage: self package
			info: self info
			snapshot: self snapshot
			dependencies: self dependencies;
		yourself