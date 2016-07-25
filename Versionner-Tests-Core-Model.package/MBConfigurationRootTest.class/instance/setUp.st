running
setUp

	| repo |
	super setUp.
	gofer := Gofer new.
	gofer disablePackageCache.
	repo := MBMonticelloPackagesResource current monticelloRepository.
	gofer repository: repo.
	Smalltalk at: #'MB_Foo_Test_Repository' put: repo.
	configurationRoot := MBConfigurationRoot new.
	configurationRoot register.
	