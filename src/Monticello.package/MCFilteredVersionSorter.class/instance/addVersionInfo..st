adding
addVersionInfo: aVersionInfo
	(aVersionInfo hasAncestor: target)
		ifTrue: [super addVersionInfo: aVersionInfo]
