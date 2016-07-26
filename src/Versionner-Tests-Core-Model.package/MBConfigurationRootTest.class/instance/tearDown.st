running
tearDown
	configurationRoot ifNotNil: [ configurationRoot unregister ].
	Smalltalk removeKey: #'MB_Foo_Test_Repository' ifAbsent: [].
	gofer := Gofer new.
	self tearDownPackages do: [:pkgName | (self hasPackage: pkgName) ifTrue: [ gofer package: pkgName ]].
	gofer references notEmpty ifTrue: [ gofer unload ].
