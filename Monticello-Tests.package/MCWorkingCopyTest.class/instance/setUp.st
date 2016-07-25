running
setUp
	| repos1 repos2 |
	self clearPackageCache.
	repositoryGroup := MCRepositoryGroup new.
	repositoryGroup disableCache.
	workingCopy := MCWorkingCopy forPackage: self mockPackage.
	versions := Dictionary new.
	versions2 := Dictionary new.
	repos1 := MCDictionaryRepository new dictionary: versions.
	repos2 := MCDictionaryRepository new dictionary: versions2.
	repositoryGroup addRepository: repos1.
	repositoryGroup addRepository: repos2.
	MCRepositoryGroup default removeRepository: repos1; removeRepository: repos2.
	workingCopy repositoryGroup: repositoryGroup.
	savedName := Author fullName.
	Author fullName: 'abc'.