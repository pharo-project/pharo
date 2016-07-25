running
tearDown
	gofer := Gofer new.
	self tearDownPackages do: [:pkgName | (self hasPackage: pkgName) ifTrue: [ gofer package: pkgName ]].
	gofer references notEmpty ifTrue: [ gofer unload ].

	Smalltalk removeKey: self repositoryName ifAbsent: [  ].
	Smalltalk removeKey: #'MB_Foo_Test_Repository' ifAbsent: [].
	
	Smalltalk at: self configurationName ifPresent: [:cls | cls removeFromSystem ].

	MetacelloPlatform current authorName: authorName.