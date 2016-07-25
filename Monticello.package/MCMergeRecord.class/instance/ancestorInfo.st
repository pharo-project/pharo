accessing
ancestorInfo
	^ ancestorInfo ifNil: [ancestorInfo := version info commonAncestorWith: version workingCopy ancestry]