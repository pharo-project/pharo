actions
withMczDo: aBlock

"	#BaselineOfPharoBootstrap asClass project version packages do: [ :package |
		aBlock value: (repository packageNamed: package name ) latestVersion mcVersion.
	]."