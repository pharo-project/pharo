accessing
info
	^ info ifNil: [ info := IceMCVersionInfo new fromCommit: self commit package: package ]