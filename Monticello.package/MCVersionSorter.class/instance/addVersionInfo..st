adding
addVersionInfo: aVersionInfo
	roots add: aVersionInfo.
	self registerStepChildrenOf: aVersionInfo seen: Set new