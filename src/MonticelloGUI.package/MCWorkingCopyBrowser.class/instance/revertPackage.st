actions
revertPackage
	self pickAncestorVersionInfo ifNotNil: [:info |
		(self repositoryGroup versionWithInfo: info
			ifNone: [^self inform: 'No repository found for ', info name]
		) load]