development support
saveModifiedPackagesIn: aConfigurationClass description: commitMessage
	"Save modified mcz files and update the configuration."

	| version |
	version := aConfigurationClass project version: #development.
	self
		saveModifiedDevelopmentPackages: version versionString for: aConfigurationClass name asString description: commitMessage;
		updateDevelopment: version versionString
			for: aConfigurationClass name asString
			updateProjects: false
			description: commitMessage