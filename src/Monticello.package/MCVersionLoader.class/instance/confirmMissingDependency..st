checking
confirmMissingDependency: aDependency
	| name |
	name := aDependency versionInfo name.
	(self confirm: 'Can''t find dependency ', name, '. ignore?')
		ifFalse: [self error: 'Can''t find dependency ', name]