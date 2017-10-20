tests
testCommit
	| repository |
	repository := MCDictionaryRepository new.
	gofer
		package: 'GoferFoo';
		load.
	gofer := Gofer new.
	gofer disablePackageCache.
	gofer repository: repository.
	gofer package: 'GoferFoo'.
	gofer commit: 'A test commit'.
	self assert: repository allVersionInfos size = 1