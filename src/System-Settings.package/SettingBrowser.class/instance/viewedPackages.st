accessing
viewedPackages
	^ viewedPackages
		ifNil: [viewedPackages := Set new]