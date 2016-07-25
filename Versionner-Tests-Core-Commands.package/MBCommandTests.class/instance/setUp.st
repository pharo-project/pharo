running
setUp

	| repo |
	super setUp.
	gofer := Gofer new.
	gofer disablePackageCache.
	repo := MBMonticelloPackagesResource current monticelloRepository.
	gofer repository: repo.
	Smalltalk at: #'MB_Foo_Test_Repository' put: repo.

	repo := MCDictionaryRepository new.
	repo description: 'dictionary://', self repositoryName asString.
	Smalltalk at: self repositoryName put: repo.

	authorName := MetacelloPlatform current authorName.
	MetacelloPlatform current authorName: 'TesterBob'.
