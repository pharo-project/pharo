accessing
realValue
	^ FileLocator origin: origin path: (RelativePath withAll: pathSegments)