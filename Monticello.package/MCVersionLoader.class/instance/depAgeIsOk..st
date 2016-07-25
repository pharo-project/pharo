checking
depAgeIsOk: aDependency
	^ aDependency isOlder not 
		or: [self confirm: 'load older dependency ', aDependency versionInfo name , '?']