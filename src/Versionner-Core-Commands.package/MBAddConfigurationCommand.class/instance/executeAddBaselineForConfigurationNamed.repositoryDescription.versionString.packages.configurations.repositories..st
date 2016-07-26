execute
executeAddBaselineForConfigurationNamed: aConfigurationClassName repositoryDescription: repositoryDescription versionString: versionString packages: packageNames configurations: configurationNames repositories: repositories
	| baselineVersionString |
	baselineVersionString := versionString , '-baseline'.
	MetacelloToolBox
		createBaseline: baselineVersionString
			for: aConfigurationClassName
			repository: repositoryDescription
			requiredProjects: configurationNames
			packages: packageNames
			repositories: repositories
			dependencies: {}
			groups: {('default' -> packageNames)};
		createDevelopment: versionString
			for: aConfigurationClassName
			importFromBaseline: baselineVersionString
			description: ''