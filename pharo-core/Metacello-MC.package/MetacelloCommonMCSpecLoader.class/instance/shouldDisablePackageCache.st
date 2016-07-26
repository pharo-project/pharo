accessing
shouldDisablePackageCache

	disablePackageCache == nil ifTrue: [ disablePackageCache := false ].
	^ disablePackageCache