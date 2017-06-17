accessing
savedPackagesDictionary
	^ savedPackages ifNil: [ savedPackages := self backend lookupPackages ]
