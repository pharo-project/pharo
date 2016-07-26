accessing
extensionDependenciesSize
	^ self dependencies
		inject: 0
		into: [ :sum :each | sum + each extensionDependenciesSize ]