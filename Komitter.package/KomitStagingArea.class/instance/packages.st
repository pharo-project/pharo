accessing
packages

	packages ifNil: [ self initializePackages ].
	^ packages copy