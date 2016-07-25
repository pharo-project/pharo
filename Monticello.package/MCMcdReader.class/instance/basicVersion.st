accessing
basicVersion
	^ MCDiffyVersion
		package: self package
		info: self info
		dependencies: self dependencies
		baseInfo: self baseInfo
		patch: self patch