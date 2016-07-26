development support
saveModifiedPackagesAndConfigurationIn: aConfigurationClass description: commitMessage
	"Save modified mcz files, update and then save the configuration."

	<apiDocumentation>
	| version |
	version := aConfigurationClass project version: #development.
	self
		saveModifiedDevelopmentPackages: version versionString
		for: aConfigurationClass name asString
		description: commitMessage.
	(self
		updateDevelopment: version versionString
		for: aConfigurationClass name asString
		updateProjects: false
		description: commitMessage) isEmpty
		ifTrue: [ self updateDescription: version versionString for: aConfigurationClass name asString description: commitMessage ].
	self saveConfigurationPackageFor: aConfigurationClass name asString description: commitMessage