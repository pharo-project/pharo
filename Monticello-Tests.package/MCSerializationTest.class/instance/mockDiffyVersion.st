mocks
mockDiffyVersion
	| repos workingCopy base next |
	repos := MCDictionaryRepository new.
	workingCopy := MCWorkingCopy forPackage: self mockPackage.
	workingCopy repositoryGroup addRepository: repos.
	MCRepositoryGroup default removeRepository: repos.
	base := self mockVersion.
	repos storeVersion: base.
	self change: #a toReturn: 'a2'.
	next := self mockVersionWithAncestor: base.
	^ next asDiffAgainst: base	