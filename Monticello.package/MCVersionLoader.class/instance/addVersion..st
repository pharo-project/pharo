loading
addVersion: aVersion
	aVersion dependencies do: [ :ea | self addDependency: ea].
	versions add: aVersion.
