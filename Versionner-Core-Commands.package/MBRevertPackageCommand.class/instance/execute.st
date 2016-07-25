execute
execute

	self pickAncestorVersionInfo ifNotNil: [:info |
		(target workingCopy repositoryGroup versionWithInfo: info
			ifNone: [^self inform: 'No repository found for ', info name]
		) load].
