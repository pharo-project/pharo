accessing
ancestorSnapshot
	^ ancestorSnapshot ifNil: [ancestorSnapshot := version workingCopy findSnapshotWithVersionInfo: self ancestorInfo]