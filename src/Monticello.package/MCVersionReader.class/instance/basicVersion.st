accessing
basicVersion
	^ MCVersion
		package: self package
		info: self info
		snapshot: [ self snapshot ]
		dependencies: self dependencies