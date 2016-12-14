stages
initializeClassPools

	shouldInitializeClassPool := true.
	environment keysDo: [ :name |
		(systemDefinition classNamed: name) isBehavior
			ifTrue: [ self initializeClassPool: name ] ].