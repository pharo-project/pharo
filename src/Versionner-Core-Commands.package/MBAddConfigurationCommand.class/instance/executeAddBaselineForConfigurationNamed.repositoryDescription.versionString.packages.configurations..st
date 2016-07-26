execute
executeAddBaselineForConfigurationNamed: aConfigurationClassName repositoryDescription: repositoryDescription versionString: versionString packages: packageNames configurations: configurationNames
	| baselineVersionString |
	baselineVersionString := versionString , '-baseline'.
	MetacelloToolBox
		createBaseline: baselineVersionString
			for: aConfigurationClassName
			repository: repositoryDescription
			requiredProjects: configurationNames
			packages: packageNames
			repositories: { aConfigurationClassName, repositoryDescription }
			dependencies: {}
			groups: {('default' -> packageNames)};
		createDevelopment: versionString
			for: aConfigurationClassName
			importFromBaseline: baselineVersionString
			description: ''