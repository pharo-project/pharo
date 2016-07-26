mc related utils
installRepository: aString for: packageName

	(self allManagers select: [:each | each package name = packageName])
		first repositoryGroup
		addRepository: (MCHttpRepository location: aString)
		