accessing
knownAncestorsOf: aVersionInfo
	^ aVersionInfo ancestors, (self stepParentsOf: aVersionInfo) asArray